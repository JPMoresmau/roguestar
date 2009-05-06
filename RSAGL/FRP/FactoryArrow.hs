{-# LANGUAGE Arrows, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, FlexibleInstances #-}

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
newtype FactoryArrow m a i o = FactoryArrow { runFactory :: m (a i o) }

instance (Category a,Monad m) => Category (FactoryArrow m a) where
    (FactoryArrow a) . (FactoryArrow b) = FactoryArrow $
        do b' <- b
           a' <- a
           return $ a' . b'
    id = FactoryArrow $ return id

instance (Arrow a,Monad m) => Arrow (FactoryArrow m a) where
    arr f = FactoryArrow $ return $ arr f
    first (FactoryArrow a) = FactoryArrow $ liftM first a
    second (FactoryArrow a) = FactoryArrow $ liftM second a

instance (Monad m,Arrow a) => ArrowTransformer (FactoryArrow m) a where
    lift = FactoryArrow . return

-- | Embed a stateful operation in an arrow.
stateContext :: (Arrow a,Monad m,MonadReference m,HasMonad m a) => b -> a (b,i) (b,o) -> FactoryArrow m a i o
stateContext initial_value actionA = FactoryArrow $
    do r <- newReference initial_value
       return $ proc i ->
           do b <- monadicAction (\() -> readReference r) -< ()
              (b',o) <- actionA -< (b,i)
              monadicAction (writeReference r) -< b'
              returnA -< o

-- | Generate a 'StatefulArrow' based on a 'FactoryArrow'.
statefulForm :: (Arrow a,ArrowApply a,Monad m,MonadReference m,HasMonad m a) => FactoryArrow m a i o -> StatefulArrow a i o
statefulForm factory = StatefulArrow $ proc i ->
    do a <- monadicAction (\() -> runFactory factory) -< ()
       let actionA = proc i' -> 
               do o <- app -< (a,i')
                  returnA -< (o,StatefulArrow actionA)
       app -< (actionA,i)

-- | A Kleisli arrow or an ArrowTransformer that can be lifted onto a Kleisli arrow.
class (Category a,Monad m) => HasMonad m a | a -> m where
    monadicAction :: (i -> m o) -> a i o

instance (Monad m) => HasMonad m (Kleisli m) where
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
