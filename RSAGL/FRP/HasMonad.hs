{-# LANGUAGE Arrows, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module RSAGL.FRP.HasMonad
    (HasMonad(..))
    where

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Monad
import Control.Category
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State

-- | A Kleisli arrow or an ArrowTransformer that can be lifted onto a Kleisli arrow.
-- TODO: Add more instances!
class (Category a) => HasMonad a where
    type MonadOf a :: * -> *
    monadicAction :: (i -> (MonadOf a) o) -> a i o

instance (Monad m) => HasMonad (Kleisli m) where
    type MonadOf (Kleisli m) = m
    monadicAction = Kleisli

instance (Arrow a,HasMonad a) => HasMonad (StateArrow s a) where
    type MonadOf (StateArrow s a) = MonadOf a
    monadicAction = lift . monadicAction

