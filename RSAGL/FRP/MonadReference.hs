{-# LANGUAGE Arrows, TypeFamilies #-}

module RSAGL.FRP.MonadReference
    (MonadReference(..))
    where

import Data.IORef
import Data.STRef
import Control.Monad.ST
import Control.Concurrent.STM

-- | A monad that has a Reference type, such as IO and IORef.
class MonadReference m where
    type Reference m :: * -> *
    newReference :: a -> m (Reference m a)
    readReference :: Reference m a -> m a
    writeReference :: Reference m a -> a -> m ()

instance MonadReference IO where
    type Reference IO = IORef
    newReference = newIORef
    readReference = readIORef
    writeReference = writeIORef

instance MonadReference (ST s) where
    type Reference (ST s) = STRef s
    newReference = newSTRef
    readReference = readSTRef
    writeReference = writeSTRef

instance MonadReference STM where
    type Reference STM = TVar
    newReference = newTVar
    readReference = readTVar
    writeReference = writeTVar
