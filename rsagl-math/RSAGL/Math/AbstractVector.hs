{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

-- | Provides generic typeclasses for common operations among many types: addition, subtraction, scalar multiplication, magnitude, and zero.
module RSAGL.Math.AbstractVector
    (AbstractVector,
     AbstractZero(..),
     AbstractAdd(..),
     AbstractSubtract(..),
     AbstractScale(..),
     AbstractMagnitude(..),
     abstractScaleTo,
     abstractSum,
     abstractAverage,
     abstractDistance)
    where

import Data.Fixed
import Control.Applicative
import RSAGL.Math.Types

-- | A data type that has an additive identity.
class AbstractZero a where
    zero :: a

-- | A data type that supports addition.
--
-- * @a `add` zero = a@
class AbstractAdd p v | p -> v where
    add :: p -> v -> p

-- | A data type that supports subtraction.
--
-- * @a `sub` a = zero@
class AbstractSubtract p v | p -> v where
    sub :: p -> p -> v

-- | A data type that supports scalar multiplication.
--
-- * @scalarMultiply 0 a = zero@
class AbstractScale v where
    scalarMultiply :: RSdouble -> v -> v

-- | A data type that supports scalar magnitude.
--
-- * @magnitude (scalarMultiply (recip $ magnitude a) a) = 1@
class AbstractMagnitude v where
    magnitude :: v -> RSdouble

-- | A convenience class for many vector types.
class (AbstractZero v,AbstractAdd v v,AbstractSubtract v v,AbstractScale v) => AbstractVector v where

-- Integer

instance AbstractZero Integer where
    zero = 0

instance AbstractAdd Integer Integer where
    add = (+)

instance AbstractSubtract Integer Integer where
    sub = (-)

instance AbstractMagnitude Integer where
    magnitude = abs . fromInteger

-- Float

instance AbstractZero Float where
    zero = 0

instance AbstractAdd Float Float where
    add = (+)

instance AbstractSubtract Float Float where
    sub = (-)

instance AbstractScale Float where
    scalarMultiply d = (f2f d *)

instance AbstractMagnitude Float where
    magnitude = abs . f2f

instance AbstractVector Float

-- Double

instance AbstractAdd Double Double where
    add = (+)

instance AbstractSubtract Double Double where
    sub = (-)

instance AbstractScale Double where
    scalarMultiply d = (f2f d *)

instance AbstractMagnitude Double where
    magnitude = abs . f2f

instance AbstractVector Double

instance AbstractZero Double where
    zero = 0

-- Fixed

instance (HasResolution a) => AbstractZero (Fixed a) where
    zero = 0

instance (HasResolution a) => AbstractAdd (Fixed a) (Fixed a) where
    add = (+)

instance (HasResolution a) => AbstractSubtract (Fixed a) (Fixed a) where
    sub = (-)

instance (HasResolution a) => AbstractScale (Fixed a) where
    scalarMultiply d = (realToFrac d *)

instance (HasResolution a) => AbstractMagnitude (Fixed a) where
    magnitude = abs . realToFrac

instance (HasResolution a) => AbstractVector (Fixed a)

-- Tuples

instance (AbstractZero a,AbstractZero b) => AbstractZero (a,b) where
    zero = (zero,zero)

instance (AbstractAdd a a',AbstractAdd b b') => AbstractAdd (a,b) (a',b') where
    add (a,b) (c,d) = (add a c,add b d)

instance (AbstractSubtract a a',AbstractSubtract b b') => AbstractSubtract (a,b) (a',b') where
    sub (a,b) (c,d) = (sub a c,sub b d)

instance (AbstractScale a,AbstractScale b) => AbstractScale (a,b) where
    scalarMultiply d (a,b) = (scalarMultiply d a,scalarMultiply d b)

instance (AbstractMagnitude a,AbstractMagnitude b) => AbstractMagnitude (a,b) where
    magnitude (a,b) = sqrt $ magnitude a ^ 2 + magnitude b ^ 2

instance (AbstractVector a,AbstractVector b) => AbstractVector (a,b)

-- Functions

instance (AbstractAdd a a') => AbstractAdd ((->) x a) ((->) x a') where
    add a b = \x -> a x `add` b x

instance (AbstractSubtract a a') => AbstractSubtract ((->) x a) ((->) x a') where
    sub a b = \x -> a x `sub` b x

instance (AbstractScale a) => AbstractScale ((->) x a) where
    scalarMultiply d f = scalarMultiply d . f

-- RSfloat

instance AbstractAdd RSfloat RSfloat where
    add = (+)

instance AbstractSubtract RSfloat RSfloat where
    sub = (-)

instance AbstractScale RSfloat where
    scalarMultiply d = (f2f d *)

instance AbstractMagnitude RSfloat where
    magnitude = abs . f2f

instance AbstractVector RSfloat

instance AbstractZero RSfloat where
    zero = 0

-- RSdouble

instance AbstractAdd RSdouble RSdouble where
    add = (+)

instance AbstractSubtract RSdouble RSdouble where
    sub = (-)

instance AbstractScale RSdouble where
    scalarMultiply = (*)

instance AbstractMagnitude RSdouble where
    magnitude = abs . f2f

instance AbstractVector RSdouble

instance AbstractZero RSdouble where
    zero = 0

-- Lists

instance (AbstractZero a) => AbstractZero [a] where
    zero = repeat zero

instance (AbstractAdd a b) => AbstractAdd [a] [b] where
    add = zipWith add

instance (AbstractSubtract a b) => AbstractSubtract [a] [b] where
    sub = zipWith sub

instance (AbstractScale a) => AbstractScale [a] where
    scalarMultiply d = map (scalarMultiply d)

instance (AbstractMagnitude a) => AbstractMagnitude [a] where
    magnitude = sqrt . sum . map ((^2) . magnitude)

instance (AbstractVector a) => AbstractVector [a] where

-- Generic functions.

-- | Force a vector to the specified magnitude.
abstractScaleTo :: (AbstractScale v,AbstractMagnitude v) => RSdouble -> v -> v
abstractScaleTo _ v | magnitude v == 0 = v
abstractScaleTo x v = scalarMultiply (x / magnitude v) v

-- | Sum of a list.
abstractSum :: (AbstractAdd p v,AbstractZero p) => [v] -> p
abstractSum = foldr (flip add) zero

-- | Average of a list.
abstractAverage :: (AbstractAdd p v,AbstractSubtract p v,AbstractVector v,AbstractZero p) => [p] -> p
abstractAverage vs = zero `add` scalarMultiply (recip $ fromInteger total_count) total_sum
    where f y (i,x) = i `seq` x `seq` (i+1,y `add` x)
          (total_count,total_sum) = foldr f (0,zero) $ map (`sub` zero) vs

-- | Distance between two points, based on the 'magnitude' of the difference.
abstractDistance :: (AbstractMagnitude v,AbstractSubtract p v) => p -> p -> RSdouble
abstractDistance x y = magnitude $ x `sub` y

