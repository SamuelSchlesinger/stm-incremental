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

This library exposes a minimal interface for incremental computation,
where we decompose our computation into the explicit computational dependencies
and only recompute things when their dependencies have been recomputed. Here is
a basic example:

@
main = do
  (a, b, c, d) <- atomically $ do
    a <- incremental 0
    b <- incremental 1
    c <- map (+ 1) b
    d <- combine (+) c b

  guard =<< (atomically $ do
    (== 3) <$> observe d)

  guard =<< (atomically $ do
    set a 1
    set b 0
    (== 1) <$> observe d)
@

Here, we see that we can set some variables and notice their changes, all done
in 'STM' 'atomically' blocks which allows us to combine these computations
transactionally with other logic we've written in 'STM'. What happens if we try
to write to @c@? Well, we get a type error! The only 'Incremental' computations
which we can set new values for are the leaves of the computation, constructed
with 'incremental'. This is this way because we don't want to mutate 'Incremental's
downstream of dependencies, because then we cannot know that those computations
remain consistent. Most operations are polymorphic over 'Mutable' and 'Immutable'
'Incremental's, but 'map', 'combine', and 'choose' all produce an @'Incremental' 'Immutable'@,
whereas 'incremental' produces a @'Incremental' 'Mutable'@.

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
  { ref :: TVar a
  , updateRef :: TVar (a -> STM ())
  }

-- | Construct a trivial, mutable incremental computation. This is mutable
-- precisely because it is trivial, as modifications to anything else
-- cannot cause it to be modified. Thus, we can change it willy nilly
-- without fear that we've invalidated someone else's changes to it.
incremental :: a -> STM (Incremental 'Mutable a)
incremental a = do
  ref <- newTVar a
  Incremental ref <$> newTVar (const (pure ()))

-- | Create an incrementally mapped computation, producing an immutable
-- incremental computation that will always contain the function mapped
-- over the value inside of the original computation.
map :: (a -> b) -> Incremental m a -> STM (Incremental 'Immutable b)
map f (Incremental ref updateRef) = do
  newRef <- readTVar ref >>= newTVar . f
  update <- readTVar updateRef
  writeTVar updateRef \a -> do
    update a
    writeTVar newRef (f a)
  Incremental newRef <$> newTVar (const (pure ()))

-- | Sets the value of a mutable incremental computation.
set :: Incremental 'Mutable a -> a -> STM ()
set incr a = do
  writeTVar (ref incr) a
  update <- readTVar (updateRef incr)
  update a

-- | Observes the present value of any incremental computation.
observe :: Incremental m a -> STM a
observe = readTVar . ref

-- | Combines the results of two incremental computation into a third,
-- producing an immutable incremental computation that will always contain
-- the function mapped over both of the values inside of the respective
-- incremental computations.
combine :: (a -> b -> c) -> Incremental m a -> Incremental m' b -> STM (Incremental 'Immutable c)
combine f (Incremental ref updateRef) (Incremental ref' updateRef') = do
  a <- readTVar ref
  b <- readTVar ref'
  newRef <- newTVar (f a b)
  update <- readTVar updateRef
  update' <- readTVar updateRef'
  writeTVar updateRef \a -> do
    update a
    b <- readTVar ref'
    writeTVar newRef (f a b)
  writeTVar updateRef' \b -> do
    update' b
    a <- readTVar ref
    writeTVar newRef (f a b)
  Incremental newRef <$> newTVar (const (pure ()))

-- | Chooses an incremental computation depending on the value inside of another
-- one. When using 'map' and 'combine', you are constructing
-- a static dependency graph, whereas when you use this function you are
-- making it dynamic, the linkages depending on the actual contents of the
-- incremental computation nodes.
choose :: Incremental m' a -> (a -> Incremental m b) -> STM (Incremental 'Immutable b)
choose (Incremental ref updateRef) f = do
  a <- readTVar ref
  let Incremental ref' updateRef' = f a
  newRef <- readTVar ref' >>= newTVar
  update' <- readTVar updateRef'
  update <- readTVar updateRef
  updateFromWhichRef <- newTVar (ref', [ref'])
  writeTVar updateRef' \b -> do
    update' b
    (tvar, _tvars) <- readTVar updateFromWhichRef
    if tvar == ref' then writeTVar newRef b
    else pure ()
  writeTVar updateRef \a -> do
    update a
    (currentRef, pastRefs) <- readTVar updateFromWhichRef
    let Incremental ref'' updateRef'' = f a
    if ref'' /= currentRef then do
      readTVar ref'' >>= writeTVar newRef
      if not (ref'' `elem` pastRefs) then do
        update'' <- readTVar updateRef''
        writeTVar updateRef'' \b -> do
          update'' b
          (tvar, _tvars) <- readTVar updateFromWhichRef
          if tvar == ref'' then writeTVar newRef b
          else pure ()
        writeTVar updateFromWhichRef (ref'', ref'' : pastRefs)
      else
        pure ()
    else pure ()
  Incremental newRef <$> newTVar (const (pure ()))
