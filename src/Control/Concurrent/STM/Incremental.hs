{-# LANGUAGE RecordWildCards #-}
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
, setEq
, map
, mapEq
, combine
, combineEq
, choose
, chooseEq
, immutable
, onUpdate
) where

import Prelude hiding (read, map)
import Data.Bool (bool)

import Control.Concurrent.STM
import Control.Monad (when)

-- Internal Documentation
--
-- An 'Incremental' consists of a 'ref', a 'TVar' which always
-- holds an up to date value that this 'Incremental' is currently equal to,
-- and an 'updateRef', a 'TVar' which always holds the code we need to run
-- upon updating this 'Incremental'. When we make an 'Incremental' using
-- 'incremental', we simply write the value in and do nothing upon update,
-- because no other 'Incremental' depends on us yet. When we make an
-- 'Incremental' using 'map', 'combine', or 'choose', we make sure to
-- modify the input 'Incremental's' 'updateRef's, as we need to update this
-- new dependent whenever we update its dependencies. Whenever we update an
-- 'Incremental' for one of these purposes, we must recursively run the
-- update code in its 'updateRef'. This allows the variables to propagate
-- outwards and allow us to observe results of the rectified computation.
--
-- We offer functions with 'Eq' suffixes which have the added optimization
-- that if the new value for an 'Incremental' is equal, we don't propagate
-- updates to its dependents. This can be used very profitably in
-- computations with subcomputations which have much smaller images than
-- domains, which often is the case.

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
  newUpdateRef <- newTVar (const (pure ()))
  update <- readTVar updateRef
  writeTVar updateRef \a -> do
    update a
    let b = f a
    writeTVar newRef b
    newUpdate <- readTVar newUpdateRef
    newUpdate b
  pure (Incremental newRef newUpdateRef)

-- | Create an incrementally mapped computation, producing an immutable
-- incremental computation that will always contain the function mapped
-- over the value inside of the original computation.
mapEq :: Eq b => (a -> b) -> Incremental m a -> STM (Incremental 'Immutable b)
mapEq f (Incremental ref updateRef) = do
  newRef <- readTVar ref >>= newTVar . f
  newUpdateRef <- newTVar (const (pure ()))
  update <- readTVar updateRef
  writeTVar updateRef \a -> do
    update a
    b <- readTVar newRef
    let b' = f a
    when (b /= b') do
      writeTVar newRef b'
      newUpdate <- readTVar newUpdateRef
      newUpdate b'
  pure (Incremental newRef newUpdateRef)

-- | Sets the value of a mutable incremental computation.
set :: Incremental 'Mutable a -> a -> STM ()
set incr a = do
  writeTVar (ref incr) a
  update <- readTVar (updateRef incr)
  update a

-- | Sets the value of a mutable incremental computation, with the added
-- optimization that if the value is equal to the old one, this does not
-- update the dependents of this 'Incremental' value.
setEq :: Eq a => Incremental 'Mutable a -> a -> STM ()
setEq incr a = do
  a' <- readTVar (ref incr)
  when (a' /= a) do
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
  newUpdateRef <- newTVar (const (pure ()))
  update <- readTVar updateRef
  update' <- readTVar updateRef'
  writeTVar updateRef \a -> do
    update a
    b <- readTVar ref'
    let c = f a b
    writeTVar newRef c
    newUpdate <- readTVar newUpdateRef
    newUpdate c
  writeTVar updateRef' \b -> do
    update' b
    a <- readTVar ref
    let c = f a b
    writeTVar newRef c
    newUpdate <- readTVar newUpdateRef
    newUpdate c
  pure (Incremental newRef newUpdateRef)

-- | Like 'combine', but with the added optimization that if the value
-- computed is equal to the old one, it will not propagate the update
-- through any further.
combineEq :: Eq c => (a -> b -> c) -> Incremental m a -> Incremental m' b -> STM (Incremental 'Immutable c)
combineEq f (Incremental ref updateRef) (Incremental ref' updateRef') = do
  a <- readTVar ref
  b <- readTVar ref'
  newRef <- newTVar (f a b)
  newUpdateRef <- newTVar (const (pure ()))
  update <- readTVar updateRef
  update' <- readTVar updateRef'
  writeTVar updateRef \a -> do
    update a
    b <- readTVar ref'
    c <- readTVar newRef
    let c' = f a b
    when (c /= c') do
      writeTVar newRef c'
      newUpdate <- readTVar newUpdateRef
      newUpdate c'
  writeTVar updateRef' \b -> do
    update' b
    a <- readTVar ref
    c <- readTVar newRef
    let c' = f a b
    when (c /= c') do
      writeTVar newRef c'
      newUpdate <- readTVar newUpdateRef
      newUpdate c'
  pure (Incremental newRef newUpdateRef)

-- | Sometimes, we need to consider an @'Incremental' 'Mutable'@ in
-- a setting alongside @'Incremental' 'Immutable'@ values, unifying their
-- type. One example where this is common is in 'choose':
--
-- @
-- ...
-- x <- incremental True
-- y <- map not x
-- z <- choose y (bool y x)
-- ...
-- @
--
-- This code will not compile, because @y@ and @x@ have different types, but this will:
--
-- @
-- ...
-- x <- incremental True
-- y <- map not x
-- z <- choose y (bool y (immutable x))
-- ...
-- @
immutable :: Incremental 'Mutable b -> Incremental 'Immutable b
immutable Incremental{..} = Incremental{..}

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
  newUpdateRef <- newTVar (const (pure ()))
  update' <- readTVar updateRef'
  update <- readTVar updateRef
  updateFromWhichRef <- newTVar (ref', [ref'])
  writeTVar updateRef' \b -> do
    update' b
    (tvar, _tvars) <- readTVar updateFromWhichRef
    when (tvar == ref') do
      writeTVar newRef b
      newUpdate <- readTVar newUpdateRef
      newUpdate b
  writeTVar updateRef \a -> do
    update a
    (currentRef, pastRefs) <- readTVar updateFromWhichRef
    let Incremental ref'' updateRef'' = f a
    when (ref'' /= currentRef) do
      b <- readTVar ref''
      writeTVar newRef b
      newUpdate <- readTVar newUpdateRef
      newUpdate b
      if not (ref'' `elem` pastRefs) then do
        update'' <- readTVar updateRef''
        writeTVar updateRef'' \b -> do
          update'' b
          (tvar, _tvars) <- readTVar updateFromWhichRef
          when (tvar == ref'') do
            writeTVar newRef b
            newUpdate <- readTVar newUpdateRef
            newUpdate b
        writeTVar updateFromWhichRef (ref'', ref'' : pastRefs)
      else writeTVar updateFromWhichRef (ref'', pastRefs)
  pure (Incremental newRef newUpdateRef)

-- | Like 'choose', but with the added optimization that we do not
-- propagate changes when we don't have to in a similar way to 'setEq',
-- 'combineEq', and 'mapEq'.
chooseEq :: Eq b => Incremental m' a -> (a -> Incremental m b) -> STM (Incremental 'Immutable b)
chooseEq (Incremental ref updateRef) f = do
  a <- readTVar ref
  let Incremental ref' updateRef' = f a
  newRef <- readTVar ref' >>= newTVar
  newUpdateRef <- newTVar (const (pure ()))
  update' <- readTVar updateRef'
  update <- readTVar updateRef
  updateFromWhichRef <- newTVar (ref', [ref'])
  writeTVar updateRef' \b -> do
    update' b
    (tvar, _tvars) <- readTVar updateFromWhichRef
    when (tvar == ref') do
      b' <- readTVar newRef
      when (b' /= b) do
        writeTVar newRef b
        newUpdate <- readTVar newUpdateRef
        newUpdate b
  writeTVar updateRef \a -> do
    update a
    (currentRef, pastRefs) <- readTVar updateFromWhichRef
    let Incremental ref'' updateRef'' = f a
    when (ref'' /= currentRef) do
      b <- readTVar ref''
      writeTVar newRef b
      newUpdate <- readTVar newUpdateRef
      newUpdate b
      if not (ref'' `elem` pastRefs) then do
        update'' <- readTVar updateRef''
        writeTVar updateRef'' \b -> do
          update'' b
          (tvar, _tvars) <- readTVar updateFromWhichRef
          when (tvar == ref'') do
            b' <- readTVar newRef
            when (b' /= b) do
              writeTVar newRef b
              newUpdate <- readTVar newUpdateRef
              newUpdate b
        writeTVar updateFromWhichRef (ref'', ref'' : pastRefs)
      else writeTVar updateFromWhichRef (ref'', pastRefs)
  pure (Incremental newRef newUpdateRef)

-- | Add monitoring hooks which can do arbitrary actions in 'STM' with the
-- changed value whenever this 'Incremental' is updated. One useful example
-- of this is used for testing this module, recording the history of an
-- 'Incremental' computation:
--
-- @
-- history :: Incremental m b -> STM (TVar [b])
-- history i = do
--   x <- observe i
--   h <- newTVar [x]
--   onUpdate i \b -> do
--     bs <- readTVar h
--     writeTVar h (b : bs)
--   pure h
-- @
onUpdate :: Incremental m b -> (b -> STM ()) -> STM ()
onUpdate (Incremental _ref updateRef) monitoring = do
  update <- readTVar updateRef
  writeTVar updateRef \b -> do
    update b
    monitoring b
    
