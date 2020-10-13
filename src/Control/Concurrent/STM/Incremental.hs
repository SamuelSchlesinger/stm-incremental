{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{- |
Module: Control.Concurrent.STM.Incremental
Description: A set of combinators for constructing and observing incremental computations.
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Control.Concurrent.STM.Incremental
( Mutability(Immutable, Mutable)
, Incremental
, incremental
, observe
, set
, map
, combine
, choose
) where

import Prelude hiding (read, map)
import Data.Bool (bool)

import Control.Concurrent.STM

-- | A data kind intended to help us expose a safe interface, only allowing
-- us to modify leaf nodes of computational graphs as to avoid inconsistent states.
data Mutability = Immutable | Mutable

-- | An incremental computation, only updated when one of its dependencies
-- is.
data Incremental (mutability :: Mutability) a = Incremental
  { read :: STM a
  , write :: a -> STM ()
  , ref :: TVar a
  , updateRef :: TVar (a -> STM ())
  }

-- | Construct a trivial, mutable incremental computation. This is mutable
-- precisely because it is trivial, as modifications to anything else
-- cannot cause it to be modified. Thus, we can change it willy nilly
-- without fear that we've invalidated someone else's changes to it.
incremental :: a -> STM (Incremental 'Mutable a)
incremental a = do
  ref <- newTVar a
  Incremental (readTVar ref) (writeTVar ref) ref <$> newTVar (const (pure ()))

-- | Create an incrementally mapped computation, producing an immutable
-- incremental computation that will always contain the function mapped
-- over the value inside of the original computation.
map :: (a -> b) -> Incremental m a -> STM (Incremental 'Immutable b)
map f (Incremental read write ref updateRef) = do
  ref <- read >>= newTVar . f
  update <- readTVar updateRef
  writeTVar updateRef \a -> do
    update a
    writeTVar ref (f a)
  Incremental (readTVar ref) (writeTVar ref) ref <$> newTVar (const (pure ()))

-- | Sets the value of a mutable incremental computation.
set :: Incremental 'Mutable a -> a -> STM ()
set incr a = do
  write incr a
  update <- readTVar (updateRef incr)
  update a

-- | Observes the present value of any incremental computation.
observe :: Incremental m a -> STM a
observe = read

-- | Combines the results of two incremental computation into a third,
-- producing an immutable incremental computation that will always contain
-- the function mapped over both of the values inside of the respective
-- incremental computations.
combine :: (a -> b -> c) -> Incremental m a -> Incremental m' b -> STM (Incremental 'Immutable c)
combine f (Incremental read ref write updateRef) (Incremental read' ref' write' updateRef') = do
  a <- read
  b <- read'
  ref <- newTVar (f a b)
  update <- readTVar updateRef
  update' <- readTVar updateRef'
  writeTVar updateRef \a -> do
    update a
    b <- read'
    writeTVar ref (f a b)
  writeTVar updateRef' \b -> do
    update' b
    a <- read
    writeTVar ref (f a b)
  Incremental (readTVar ref) (writeTVar ref) ref <$> newTVar (const (pure ()))

-- | Chooses an incremental computation depending on the value inside of another
-- one. When using 'map' and 'combine', you are constructing
-- a static dependency graph, whereas when you use this function you are
-- making it dynamic, the linkages depending on the actual contents of the
-- incremental computation nodes.
choose :: Incremental m' a -> (a -> Incremental m b) -> STM (Incremental 'Immutable b)
choose (Incremental read write ref updateRef) f = do
  a <- read
  let Incremental read' write' ref' updateRef' = f a
  ref <- read' >>= newTVar
  update' <- readTVar updateRef'
  update <- readTVar updateRef
  updateFromWhichRef <- newTVar (ref', [ref'])
  writeTVar updateRef' \b -> do
    update' b
    (tvar, _tvars) <- readTVar updateFromWhichRef
    if tvar == ref' then writeTVar ref b
    else pure ()
  writeTVar updateRef \a -> do
    update a
    (currentRef, pastRefs) <- readTVar updateFromWhichRef
    let Incremental read'' write'' ref'' updateRef'' = f a
    if ref'' /= currentRef then do
      read'' >>= writeTVar ref
      if not (ref'' `elem` pastRefs) then do
        update'' <- readTVar updateRef''
        writeTVar updateRef'' \b -> do
          update'' b
          (tvar, _tvars) <- readTVar updateFromWhichRef
          if tvar == ref'' then writeTVar ref b
          else pure ()
        writeTVar updateFromWhichRef (ref'', ref'' : pastRefs)
      else
        pure ()
    else pure ()
  Incremental (readTVar ref) (writeTVar ref) ref <$> newTVar (const (pure ()))
