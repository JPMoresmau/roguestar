{-# LANGUAGE Arrows, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ExistentialQuantification, Rank2Types #-}

module RSAGL.FRP2.FactoryArrow
    (FactoryArrow(..))
    where

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Category

-- | An 'Arrow' that constructs an associated monadic computation.
newtype FactoryArrow m n i o = FactoryArrow { runFactory :: m (Kleisli n i o) }

instance (Monad m,Monad n) => Category (FactoryArrow m n) where
    (FactoryArrow a) . (FactoryArrow b) = FactoryArrow $
        do b' <- b
           a' <- a
           return $ a' . b'
    id = FactoryArrow $ return id

instance (Monad m,Monad n) => Arrow (FactoryArrow m n) where
    arr = FactoryArrow . return . arr
    first = FactoryArrow . liftM first . runFactory
    second = FactoryArrow . liftM second . runFactory

instance (Monad m,MonadFix n) => ArrowLoop (FactoryArrow m n) where
    loop = FactoryArrow . liftM loop . runFactory

-- | Careful!  To implement ArrowApply, the factory action must run imbedded in the constructed action.
instance (Monad m) => ArrowApply (FactoryArrow m m) where
    app = factoryApp id

-- | Implements ArrowApply for any FactoryArrow capable of it, 
-- but this requires a way to lift operations in m into n.
factoryApp :: (Monad m,Monad n) => (forall a. m a -> n a) -> FactoryArrow m n (FactoryArrow m n i o,i) o
factoryApp liftM2N = FactoryArrow $ return $ Kleisli $ \(FactoryArrow m,i) ->
    do (Kleisli n) <- liftM2N m
       n i

-- | A choice is constructed at factory time whether or not the constructed action is ever evaluated.
instance (Monad m,Monad n) => ArrowChoice (FactoryArrow m n) where
    left = FactoryArrow . liftM left . runFactory
    right = FactoryArrow . liftM right . runFactory

instance (Monad m,MonadPlus n) => ArrowZero (FactoryArrow m n) where
    zeroArrow = FactoryArrow $ return zeroArrow

-- | As with ArrowChoice, both branches are constructed at factory time whether or not the constructed actions are ever evaluated.
instance (Monad m,MonadPlus n) => ArrowPlus (FactoryArrow m n) where
    a <+> b = FactoryArrow $ liftM2 (<+>) (runFactory a) (runFactory b)

