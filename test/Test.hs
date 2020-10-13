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
  it "Binds alright" do
    (x, y, z, a) <- atomically do
      x <- incremental 100
      y <- incremental 200
      z <- incremental True
      a <- choose z (bool x y)
      pure (x, y, z, a)
    atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a) `shouldReturn` (100, 200, True, 200)
    atomically (set x 0)
    atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a) `shouldReturn` (0, 200, True, 200)
    atomically (set y 100)
    atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a) `shouldReturn` (0, 100, True, 100)
    atomically (set z False)
    atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a) `shouldReturn` (0, 100, False, 0)
    atomically (set x 1000)
    atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a) `shouldReturn` (1000, 100, False, 1000)
    atomically (set y 500)
    atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a) `shouldReturn` (1000, 500, False, 1000) 
    atomically (set z True)
    atomically ((,,,) <$> observe x <*> observe y <*> observe z <*> observe a) `shouldReturn` (1000, 500, True, 500)
  it "maps and combines alright" do
    (x, y, z) <- atomically do
      x <- incremental 100
      y <- map (+ 10) x
      z <- combine (+) x y
      pure (x, y, z)
    atomically ((,,) <$> observe x <*> observe y <*> observe z) `shouldReturn` (100, 110, 210)
    atomically (set x 10)
    atomically ((,,) <$> observe x <*> observe y <*> observe z) `shouldReturn` (10, 20, 30) 
  it "combines alright" do
    (x, y, z) <- atomically do
      salutation <- incremental "Heya"
      name <- incremental "Samuel"
      greeting <- combine (\s n -> s <> ", " <> n) salutation name
      pure (salutation, name, greeting)
    atomically ((,,) <$> observe x <*> observe y <*> observe z) `shouldReturn` ("Heya", "Samuel", "Heya, Samuel")
    atomically (set x "Hello")
    atomically ((,,) <$> observe x <*> observe y <*> observe z) `shouldReturn` ("Hello", "Samuel", "Hello, Samuel")
  it "maps alright" do
    (x, y) <- atomically do
      x <- incremental "Hello, world"
      y <- map ((<> "!") . fmap toUpper) x
      pure (x, y)
    atomically ((,) <$> observe x <*> observe y) `shouldReturn` ("Hello, world", "HELLO, WORLD!")
    atomically (set x "Wot, m8?")
    atomically ((,) <$> observe x <*> observe y) `shouldReturn` ("Wot, m8?", "WOT, M8?!")
