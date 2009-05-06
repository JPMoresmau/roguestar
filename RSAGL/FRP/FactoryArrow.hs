{-# LANGUAGE Arrows, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module RSAGL.FRP.FactoryArrow
    (FactoryArrow(..),
     RSAGL.FRP.FactoryArrow.stateContext,
     RSAGL.FRP.FactoryArrow.statefulForm,
     MonadReference(..),
     HasMonad(..))
    where

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Monad
import Control.Category
import Control.Arrow.Transformer
import Data.IORef
import RSAGL.FRP.StatefulArrow

-- | An 'Arrow' that generates another 'Arrow' inside a 'Monad'.
newtype (HasMonad a) => FactoryArrow a i o = FactoryArrow { runFactory :: (MonadOf a) (a i o) }

instance (Category a,HasMonad a,Monad (MonadOf a)) => Category (FactoryArrow a) where
    (FactoryArrow a) . (FactoryArrow b) = FactoryArrow $
        do b' <- b
           a' <- a
           return $ a' . b'
    id = FactoryArrow $ return id

instance (Arrow a,HasMonad a,Monad (MonadOf a)) => Arrow (FactoryArrow a) where
    arr f = FactoryArrow $ return $ arr f
    first (FactoryArrow a) = FactoryArrow $ liftM first a
    second (FactoryArrow a) = FactoryArrow $ liftM second a

instance (Arrow a,HasMonad a,Monad (MonadOf a)) => ArrowTransformer FactoryArrow a where
    lift = FactoryArrow . return

-- | Embed a stateful operation in an arrow.
stateContext :: (Arrow a,HasMonad a,Monad (MonadOf a),MonadReference (MonadOf a)) => b -> a (b,i) (b,o) -> FactoryArrow a i o
stateContext initial_value actionA = FactoryArrow $
    do r <- newReference initial_value
       return $ proc i ->
           do b <- monadicAction (\() -> readReference r) -< ()
              (b',o) <- actionA -< (b,i)
              monadicAction (writeReference r) -< b'
              returnA -< o

-- | Generate a 'StatefulArrow' based on a 'FactoryArrow'.
statefulForm :: (Arrow a,ArrowApply a,MonadReference (MonadOf a),HasMonad a) => FactoryArrow a i o -> StatefulArrow a i o
statefulForm factory = StatefulArrow $ proc i ->
    do a <- monadicAction (\() -> runFactory factory) -< ()
       let actionA = proc i' -> 
               do o <- app -< (a,i')
                  returnA -< (o,StatefulArrow actionA)
       app -< (actionA,i)

-- | A Kleisli arrow or an ArrowTransformer that can be lifted onto a Kleisli arrow.
class (Category a) => HasMonad a where
    type MonadOf a :: * -> *
    monadicAction :: (i -> (MonadOf a) o) -> a i o

instance (Monad m) => HasMonad (Kleisli m) where
    type MonadOf (Kleisli m) = m
    monadicAction = Kleisli

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
