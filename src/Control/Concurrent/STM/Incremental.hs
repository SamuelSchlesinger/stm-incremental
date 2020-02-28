{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Concurrent.STM.Incremental where

import Control.Concurrent.STM

data Incremental a = Incremental { contents :: TVar a, onUpdate :: TVar (STM ()) }

imap :: (a -> b) -> Incremental a -> STM (Incremental b)
imap f (Incremental contents onUpdate) = do
  a <- readTVar contents
  contents' <- newTVar (f a)
  modifyTVar onUpdate (>> do
    a' <- readTVar contents
    writeTVar contents' (f a'))
  onUpdate' <- newTVar (pure ())
  return (Incremental contents' onUpdate')

iliftA2 :: (a -> b -> c) -> Incremental a -> Incremental b -> STM (Incremental c)
iliftA2 f (Incremental contents onUpdate) (Incremental contents' onUpdate') = do
  a <- readTVar contents
  b <- readTVar contents'
  contents'' <- newTVar (f a b)
  modifyTVar onUpdate (modifyOnUpdate contents'')
  modifyTVar onUpdate' (modifyOnUpdate contents'')
  onUpdate'' <- newTVar (pure ())
  return (Incremental contents'' onUpdate'')
  where
  modifyOnUpdate contents'' a = a >> do
    a' <- readTVar contents
    b' <- readTVar contents'
    writeTVar contents'' (f a' b')

imodify :: Incremental a -> (a -> a) -> STM ()
imodify (Incremental contents onUpdate) f = do
  modifyTVar contents f
  update <- readTVar onUpdate
  update

iread :: Incremental a -> STM a
iread (Incremental contents _) = readTVar contents

inc :: a -> STM (Incremental a)
inc a = Incremental <$> newTVar a <*> newTVar (pure ())
