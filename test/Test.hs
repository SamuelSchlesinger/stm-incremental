{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Concurrent.STM.Incremental
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

main :: IO ()
main = (>> putStrLn "Success!") $ do
  test_iliftA2 10
  test_imap 10

test_imap :: Int -> IO ()
test_imap size = forM_ [1..size] \n -> forM_ [1..size] \m -> do
  ix <- atomically $ inc 0
  iy <- atomically $ imap (+ n) ix
  x <- atomically $ iread ix
  y <- atomically $ iread iy
  guard (x == 0 && y == n)
  atomically $ imodify ix (+ m)
  x' <- atomically $ iread ix
  y' <- atomically $ iread iy
  guard (x' == m && y' == n + m)

test_iliftA2 :: Int -> IO ()
test_iliftA2 size = forM_ [1..size] \n -> forM_ [1..size] \m -> forM_ [1..size] \l -> do
  ix <- atomically $ inc 0
  iy <- atomically $ inc n
  iz <- atomically $ iliftA2 (+) ix iy
  z <- atomically $ iread iz
  atomically $ imodify ix (+ m)
  z' <- atomically $ iread iz
  guard (z + m == z')
  atomically $ imodify iy (+ l)
  z'' <- atomically $ iread iz
  guard (z' + l == z'')
