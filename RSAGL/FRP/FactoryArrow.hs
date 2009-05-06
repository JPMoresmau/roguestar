{-# LANGUAGE Arrows, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module RSAGL.FRP.FactoryArrow
    (FactoryArrow(..),
     RSAGL.FRP.FactoryArrow.stateContext,
     RSAGL.FRP.FactoryArrow.statefulForm)
    where

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Monad
import Control.Category
import Control.Arrow.Transformer
import RSAGL.FRP.StatefulArrow
import RSAGL.FRP.HasMonad
import RSAGL.FRP.MonadReference

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

