{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Prelude hiding (map)
import Data.Bool (bool)
import Data.Char (toUpper)

import Control.Concurrent.STM.Incremental
import Control.Concurrent.STM
import Test.Hspec


main :: IO ()
main = hspec . describe "stm-incremental" $ do
  it "nests binds right" do
    (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist, h, hHist) <- atomically do
      a <- incremental "a"
      aHist <- history a
      b <- incremental "b"
      bHist <- history b
      c <- incremental True
      cHist <- history c
      d <- choose c (bool a b)
      dHist <- history d
      e <- map (== "a") d
      eHist <- history e
      f <- choose e (bool (immutable b) d)
      fHist <- history f
      g <- choose f \case
        "a" -> a
        "b" -> b
      gHist <- history g
      h <- choose g \case
        "a" -> immutable c
        "b" -> e
      hHist <- history h
      pure (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist, h, hHist)
    let recv = atomically ((,,,,,,,) <$> observe a <*> observe b <*> observe c <*> observe d <*> observe e <*> observe f <*> observe g <*> observe h)
    recv `shouldReturn` ("a", "b", True, "b", False, "b", "b", False)
    atomically (set b "a")
    recv `shouldReturn` ("a", "a", True, "a", True, "a", "a", True)
    atomically (set c False)
    recv `shouldReturn` ("a", "a", False, "a", True, "a", "a", False)
    atomically (set a "b")
    recv `shouldReturn` ("b", "a", False, "b", False, "a", "b", False)
    atomically aHist `shouldReturn` ["b", "a"]
    atomically bHist `shouldReturn` ["a", "b"]
    atomically cHist `shouldReturn` [False, True]
    atomically dHist `shouldReturn` ["b", "a", "a", "b"]
    atomically eHist `shouldReturn` [False, True, True, False]
    atomically fHist `shouldReturn` ["a", "a", "a", "b"]
    atomically gHist `shouldReturn` ["b", "a", "b"]
    atomically hHist `shouldReturn` [False, True, False, True, False]
  it "nests combines right" do
    (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist) <- atomically do
      a <- incremental "X"
      aHist <- history a
      b <- incremental "O"
      bHist <- history b
      c <- incremental "Z"
      cHist <- history c
      d <- combine (<>) a c
      dHist <- history d
      e <- combine (<>) c b
      eHist <- history e
      f <- combine (<>) d e
      fHist <- history f
      g <- combine (<>) a f
      gHist <- history g
      pure (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist)
    let recv = atomically ((,,,,,,) <$> observe a <*> observe b <*> observe c <*> observe d <*> observe e <*> observe f <*> observe g)
    recv `shouldReturn` ("X", "O", "Z", "XZ", "ZO", "XZZO", "XXZZO")
    atomically (set a "A")
    recv `shouldReturn` ("A", "O", "Z", "AZ", "ZO", "AZZO", "AAZZO")
    atomically (set b "D")
    recv `shouldReturn` ("A", "D", "Z", "AZ", "ZD", "AZZD", "AAZZD")
    atomically (set c "P")
    recv `shouldReturn` ("A", "D", "P", "AP", "PD", "APPD", "AAPPD")
    atomically aHist `shouldReturn` ["A","X"]
    atomically bHist `shouldReturn` ["D","O"]
    atomically cHist `shouldReturn` ["P", "Z"]
    atomically dHist `shouldReturn` ["AP","AZ","XZ"]
    atomically eHist `shouldReturn` ["PD","ZD","ZO"]
    atomically fHist `shouldReturn` ["APPD","APZD","AZZD","AZZO","XZZO"]
    atomically gHist `shouldReturn` ["AAPPD","AAPZD","AAZZD","AAZZO","AAZZO","XXZZO"]
  it "nests maps right" do
    (a, aHist, b, bHist, c, cHist, d, dHist) <- atomically do
      a <- incremental 1
      aHist <- history a
      b <- map (+ 1) a
      bHist <- history b
      c <- map (+ 1) b
      cHist <- history c
      d <- map (+ 1) c
      dHist <- history d
      pure (a, aHist, b, bHist, c, cHist, d, dHist)
    let recv = atomically ((,,,) <$> observe a <*> observe b <*> observe c <*> observe d)
    recv `shouldReturn` (1, 2, 3, 4)
    atomically (set a 101)
    recv `shouldReturn` (101, 102, 103, 104) 
    atomically aHist `shouldReturn` [101,1]
    atomically bHist `shouldReturn` [102, 2]
    atomically cHist `shouldReturn` [103, 3]
    atomically dHist `shouldReturn` [104, 4]
  it "binds alright" do
    (x, xHist, y, yHist, z, zHist, a, aHist) <- atomically do
      x <- incremental 100
      xHist <- history x
      y <- incremental 200
      yHist <- history y
      z <- incremental True
      zHist <- history z
      a <- choose z (bool x y)
      aHist <- history a
      pure (x, xHist, y, yHist, z, zHist, a, aHist)
    let recv = atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a)
    recv `shouldReturn` (100, 200, True, 200)
    atomically (set x 0)
    recv `shouldReturn` (0, 200, True, 200)
    atomically (set y 100)
    recv `shouldReturn` (0, 100, True, 100)
    atomically (set z False)
    recv `shouldReturn` (0, 100, False, 0)
    atomically (set x 1000)
    recv `shouldReturn` (1000, 100, False, 1000)
    atomically (set y 500)
    recv `shouldReturn` (1000, 500, False, 1000) 
    atomically (set z True)
    recv `shouldReturn` (1000, 500, True, 500)
    atomically xHist `shouldReturn` [1000,0,100]
    atomically yHist `shouldReturn` [500, 100, 200] 
    atomically zHist `shouldReturn` [True, False, True]
    atomically aHist `shouldReturn` [500,1000,0,100,200]
  it "maps and combines alright" do
    (x, y, z) <- atomically do
      x <- incremental 100
      y <- map (+ 10) x
      z <- combine (+) x y
      pure (x, y, z)
    let recv = atomically ((,,) <$> observe x <*> observe y <*> observe z)
    recv `shouldReturn` (100, 110, 210)
    atomically (set x 10)
    recv `shouldReturn` (10, 20, 30) 
  it "combines alright" do
    (x, xHistory, y, yHistory, z, zHistory) <- atomically do
      salutation <- incremental "Heya"
      salutationHistory <- history salutation
      name <- incremental "Samuel"
      nameHistory <- history name
      greeting <- combine (\s n -> s <> ", " <> n) salutation name
      greetingHistory <- history greeting
      pure (salutation, salutationHistory, name, nameHistory, greeting, greetingHistory)
    let recv = atomically ((,,) <$> observe x <*> observe y <*> observe z)
    recv `shouldReturn` ("Heya", "Samuel", "Heya, Samuel")
    atomically (set x "Hello")
    recv `shouldReturn` ("Hello", "Samuel", "Hello, Samuel")
    atomically xHistory `shouldReturn` ["Hello", "Heya"]
    atomically yHistory `shouldReturn` ["Samuel"]
    atomically zHistory `shouldReturn` ["Hello, Samuel", "Heya, Samuel"]
  it "maps alright" do
    (x, xHistory, y, yHistory) <- atomically do
      x <- incremental "Hello, world"
      xHistory <- history x
      y <- map ((<> "!") . fmap toUpper) x
      yHistory <- history y
      pure (x, xHistory, y, yHistory)
    let recv = atomically ((,) <$> observe x <*> observe y) 
    recv `shouldReturn` ("Hello, world", "HELLO, WORLD!")
    atomically (set x "Wot, m8?")
    atomically (set x "Wot, m8?")
    recv `shouldReturn` ("Wot, m8?", "WOT, M8?!")
    atomically xHistory `shouldReturn` ["Wot, m8?", "Wot, m8?", "Hello, world"]
    atomically yHistory `shouldReturn` ["WOT, M8?!", "WOT, M8?!", "HELLO, WORLD!"]
  describe "with eq operations" do
    it "nests binds right" do
      (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist, h, hHist) <- atomically do
        a <- incremental "a"
        aHist <- history a
        b <- incremental "b"
        bHist <- history b
        c <- incremental True
        cHist <- history c
        d <- chooseEq c (bool a b)
        dHist <- history d
        e <- mapEq (== "a") d
        eHist <- history e
        f <- chooseEq e (bool (immutable b) d)
        fHist <- history f
        g <- chooseEq f \case
          "a" -> a
          "b" -> b
        gHist <- history g
        h <- chooseEq g \case
          "a" -> immutable c
          "b" -> e
        hHist <- history h
        pure (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist, h, hHist)
      let recv = atomically ((,,,,,,,) <$> observe a <*> observe b <*> observe c <*> observe d <*> observe e <*> observe f <*> observe g <*> observe h)
      recv `shouldReturn` ("a", "b", True, "b", False, "b", "b", False)
      atomically (setEq b "a")
      recv `shouldReturn` ("a", "a", True, "a", True, "a", "a", True)
      atomically (setEq c False)
      recv `shouldReturn` ("a", "a", False, "a", True, "a", "a", False)
      atomically (setEq a "b")
      recv `shouldReturn` ("b", "a", False, "b", False, "a", "b", False)
      atomically aHist `shouldReturn` ["b", "a"]
      atomically bHist `shouldReturn` ["a", "b"]
      atomically cHist `shouldReturn` [False, True]
      atomically dHist `shouldReturn` ["b", "a", "a", "b"]
      atomically eHist `shouldReturn` [False,True,False]
      atomically fHist `shouldReturn` ["a", "a", "b"]
      atomically gHist `shouldReturn` ["b", "a", "b"]
      atomically hHist `shouldReturn` [False, True, False, True, False]
    it "nests combines right" do
      (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist) <- atomically do
        a <- incremental "X"
        aHist <- history a
        b <- incremental "O"
        bHist <- history b
        c <- incremental "Z"
        cHist <- history c
        d <- combineEq (<>) a c
        dHist <- history d
        e <- combineEq (<>) c b
        eHist <- history e
        f <- combineEq (<>) d e
        fHist <- history f
        g <- combineEq (<>) a f
        gHist <- history g
        pure (a, aHist, b, bHist, c, cHist, d, dHist, e, eHist, f, fHist, g, gHist)
      let recv = atomically ((,,,,,,) <$> observe a <*> observe b <*> observe c <*> observe d <*> observe e <*> observe f <*> observe g)
      recv `shouldReturn` ("X", "O", "Z", "XZ", "ZO", "XZZO", "XXZZO")
      atomically (setEq a "A")
      recv `shouldReturn` ("A", "O", "Z", "AZ", "ZO", "AZZO", "AAZZO")
      atomically (setEq b "D")
      recv `shouldReturn` ("A", "D", "Z", "AZ", "ZD", "AZZD", "AAZZD")
      atomically (setEq c "P")
      recv `shouldReturn` ("A", "D", "P", "AP", "PD", "APPD", "AAPPD")
      atomically aHist `shouldReturn` ["A","X"]
      atomically bHist `shouldReturn` ["D","O"]
      atomically cHist `shouldReturn` ["P", "Z"]
      atomically dHist `shouldReturn` ["AP","AZ","XZ"]
      atomically eHist `shouldReturn` ["PD","ZD","ZO"]
      atomically fHist `shouldReturn` ["APPD","APZD","AZZD","AZZO","XZZO"]
      atomically gHist `shouldReturn` ["AAPPD","AAPZD","AAZZD","AAZZO","XXZZO"]
    it "nests maps right" do
      (a, aHist, b, bHist, c, cHist, d, dHist) <- atomically do
        a <- incremental 1
        aHist <- history a
        b <- mapEq (+ 1) a
        bHist <- history b
        c <- mapEq (+ 1) b
        cHist <- history c
        d <- mapEq (+ 1) c
        dHist <- history d
        pure (a, aHist, b, bHist, c, cHist, d, dHist)
      let recv = atomically ((,,,) <$> observe a <*> observe b <*> observe c <*> observe d)
      recv `shouldReturn` (1, 2, 3, 4)
      atomically (setEq a 101)
      recv `shouldReturn` (101, 102, 103, 104) 
      atomically aHist `shouldReturn` [101,1]
      atomically bHist `shouldReturn` [102, 2]
      atomically cHist `shouldReturn` [103, 3]
      atomically dHist `shouldReturn` [104, 4]
    it "binds alright" do
      (x, xHist, y, yHist, z, zHist, a, aHist) <- atomically do
        x <- incremental 100
        xHist <- history x
        y <- incremental 200
        yHist <- history y
        z <- incremental True
        zHist <- history z
        a <- chooseEq z (bool x y)
        aHist <- history a
        pure (x, xHist, y, yHist, z, zHist, a, aHist)
      let recv = atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a)
      recv `shouldReturn` (100, 200, True, 200)
      atomically (setEq x 0)
      recv `shouldReturn` (0, 200, True, 200)
      atomically (setEq y 100)
      recv `shouldReturn` (0, 100, True, 100)
      atomically (setEq z False)
      recv `shouldReturn` (0, 100, False, 0)
      atomically (setEq x 1000)
      recv `shouldReturn` (1000, 100, False, 1000)
      atomically (setEq y 500)
      recv `shouldReturn` (1000, 500, False, 1000) 
      atomically (setEq z True)
      recv `shouldReturn` (1000, 500, True, 500)
      atomically xHist `shouldReturn` [1000,0,100]
      atomically yHist `shouldReturn` [500, 100, 200] 
      atomically zHist `shouldReturn` [True, False, True]
      atomically aHist `shouldReturn` [500,1000,0,100,200]
    it "maps and combines alright" do
      (x, y, z) <- atomically do
        x <- incremental 100
        y <- mapEq (+ 10) x
        z <- combineEq (+) x y
        pure (x, y, z)
      let recv = atomically ((,,) <$> observe x <*> observe y <*> observe z)
      recv `shouldReturn` (100, 110, 210)
      atomically (setEq x 10)
      recv `shouldReturn` (10, 20, 30) 
    it "combines alright" do
      (x, xHistory, y, yHistory, z, zHistory) <- atomically do
        salutation <- incremental "Heya"
        salutationHistory <- history salutation
        name <- incremental "Samuel"
        nameHistory <- history name
        greeting <- combineEq (\s n -> s <> ", " <> n) salutation name
        greetingHistory <- history greeting
        pure (salutation, salutationHistory, name, nameHistory, greeting, greetingHistory)
      let recv = atomically ((,,) <$> observe x <*> observe y <*> observe z)
      recv `shouldReturn` ("Heya", "Samuel", "Heya, Samuel")
      atomically (setEq x "Hello")
      recv `shouldReturn` ("Hello", "Samuel", "Hello, Samuel")
      atomically xHistory `shouldReturn` ["Hello", "Heya"]
      atomically yHistory `shouldReturn` ["Samuel"]
      atomically zHistory `shouldReturn` ["Hello, Samuel", "Heya, Samuel"]
    it "maps alright" do
      (x, xHistory, y, yHistory) <- atomically do
        x <- incremental "Hello, world"
        xHistory <- history x
        y <- mapEq ((<> "!") . fmap toUpper) x
        yHistory <- history y
        pure (x, xHistory, y, yHistory)
      let recv = atomically ((,) <$> observe x <*> observe y) 
      recv `shouldReturn` ("Hello, world", "HELLO, WORLD!")
      atomically (setEq x "Wot, m8?")
      atomically (setEq x "Wot, m8?")
      recv `shouldReturn` ("Wot, m8?", "WOT, M8?!")
      atomically xHistory `shouldReturn` ["Wot, m8?", "Hello, world"]
      atomically yHistory `shouldReturn` ["WOT, M8?!", "HELLO, WORLD!"]
