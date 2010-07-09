{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}

module RSAGL.Auxiliary.ApplicativeWrapper
    (ApplicativeWrapper(..),
     fromPure,
     toApplicative,
     unwrapApplicative,
     wrapApplicative,
     isPure)
    where

import Control.Applicative
import Data.Maybe
import Control.Parallel.Strategies
import RSAGL.Math.AbstractVector

-- | 'ApplicativeWrapper' captures an applicative entity and remembers whether the entity is pure.
newtype ApplicativeWrapper f a = ApplicativeWrapper (Either (f a) a)

instance (Functor f,Applicative f) => Functor (ApplicativeWrapper f) where
    fmap f (ApplicativeWrapper (Right a)) = pure $ f a
    fmap f (ApplicativeWrapper (Left a)) = wrapApplicative $ fmap f a

instance (Applicative f) => Applicative (ApplicativeWrapper f) where
    pure = ApplicativeWrapper . Right
    (ApplicativeWrapper (Right f)) <*> (ApplicativeWrapper (Right a)) = ApplicativeWrapper $ Right $ f a
    f <*> a = wrapApplicative $ toApplicative f <*> toApplicative a

instance (NFData (f a),NFData a) => NFData (ApplicativeWrapper f a) where
    rnf (ApplicativeWrapper ethr) = rnf ethr

instance (Applicative f,AbstractZero p) => AbstractZero (ApplicativeWrapper f p) where
    zero = pure zero

instance (Applicative f,AbstractAdd p v) => AbstractAdd (ApplicativeWrapper f p) (ApplicativeWrapper f v) where
    add p v = add <$> p <*> v

instance (Applicative f,AbstractSubtract p v) => AbstractSubtract (ApplicativeWrapper f p) (ApplicativeWrapper f v) where
    sub x y = sub <$> x <*> y

instance (Applicative f,AbstractScale v) => AbstractScale (ApplicativeWrapper f v) where
    scalarMultiply d v = scalarMultiply d <$> v

instance (Applicative f,AbstractVector v) => AbstractVector (ApplicativeWrapper f v)

fromPure :: (Applicative f) => ApplicativeWrapper f a -> Maybe a
fromPure = either (const Nothing) Just . unwrapApplicative

toApplicative :: (Applicative f) => ApplicativeWrapper f a -> f a
toApplicative = either id pure . unwrapApplicative

unwrapApplicative :: (Applicative f) => ApplicativeWrapper f a -> Either (f a) a
unwrapApplicative (ApplicativeWrapper x) = x

wrapApplicative :: (Applicative f) => f a -> ApplicativeWrapper f a
wrapApplicative = ApplicativeWrapper . Left

isPure :: (Applicative f) => ApplicativeWrapper f a -> Bool
isPure = isJust . fromPure
