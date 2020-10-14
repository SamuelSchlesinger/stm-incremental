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
    (a, b, c, d, e, f, g) <- atomically do
      a <- incremental "a"
      b <- incremental "b"
      c <- incremental True
      d <- choose c (bool a b)
      e <- map (== "a") d
      f <- choose e (bool (immutable b) d)
      g <- choose f \case
        "a" -> a
        "b" -> b
      pure (a, b, c, d, e, f, g)
    let recv = atomically ((,,,,,,) <$> observe a <*> observe b <*> observe c <*> observe d <*> observe e <*> observe f <*> observe g)
    recv `shouldReturn` ("a", "b", True, "b", False, "b", "b")
    atomically (set b "a")
    recv `shouldReturn` ("a", "a", True, "a", True, "a", "a")
    atomically (set c False)
    recv `shouldReturn` ("a", "a", False, "a", True, "a", "a")
    atomically (set a "b")
    recv `shouldReturn` ("b", "a", False, "b", False, "a", "b")
      
  it "nests combines right" do
    (a, b, c, d, e, f, g) <- atomically do
      a <- incremental "X"
      b <- incremental "O"
      c <- incremental "Z"
      d <- combine (<>) a c
      e <- combine (<>) c b
      f <- combine (<>) d e
      g <- combine (<>) a f
      pure (a, b, c, d, e, f, g)
    let recv = atomically ((,,,,,,) <$> observe a <*> observe b <*> observe c <*> observe d <*> observe e <*> observe f <*> observe g)
    recv `shouldReturn` ("X", "O", "Z", "XZ", "ZO", "XZZO", "XXZZO")
    atomically (set a "A")
    recv `shouldReturn` ("A", "O", "Z", "AZ", "ZO", "AZZO", "AAZZO")
    atomically (set b "D")
    recv `shouldReturn` ("A", "D", "Z", "AZ", "ZD", "AZZD", "AAZZD")
    atomically (set c "P")
    recv `shouldReturn` ("A", "D", "P", "AP", "PD", "APPD", "AAPPD")
  it "nests maps right" do
    (a, b, c, d) <- atomically do
      a <- incremental 1
      b <- map (+ 1) a
      c <- map (+ 1) b
      d <- map (+ 1) c
      pure (a, b, c, d)
    let recv = atomically ((,,,) <$> observe a <*> observe b <*> observe c <*> observe d)
    recv `shouldReturn` (1, 2, 3, 4)
    atomically (set a 101)
    recv `shouldReturn` (101, 102, 103, 104) 
  it "binds alright" do
    (x, y, z, a) <- atomically do
      x <- incremental 100
      y <- incremental 200
      z <- incremental True
      a <- choose z (bool x y)
      pure (x, y, z, a)
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
    (x, y, z) <- atomically do
      salutation <- incremental "Heya"
      name <- incremental "Samuel"
      greeting <- combine (\s n -> s <> ", " <> n) salutation name
      pure (salutation, name, greeting)
    let recv = atomically ((,,) <$> observe x <*> observe y <*> observe z)
    recv `shouldReturn` ("Heya", "Samuel", "Heya, Samuel")
    atomically (set x "Hello")
    recv `shouldReturn` ("Hello", "Samuel", "Hello, Samuel")
  it "maps alright" do
    (x, y) <- atomically do
      x <- incremental "Hello, world"
      y <- map ((<> "!") . fmap toUpper) x
      pure (x, y)
    let recv = atomically ((,) <$> observe x <*> observe y) 
    recv `shouldReturn` ("Hello, world", "HELLO, WORLD!")
    atomically (set x "Wot, m8?")
    recv `shouldReturn` ("Wot, m8?", "WOT, M8?!")
