{-# LANGUAGE MultiParamTypeClasses #-}
module RSAGL.Color.Alpha
    (Alpha(..),alpha,alpha256)
    where

import RSAGL.Types
import RSAGL.Color.Auxiliary
import RSAGL.Math.AbstractVector
import Control.Parallel.Strategies
import Control.Applicative
import RSAGL.Color.ColorSpace

-- | A color with an alpha channel.
data Alpha c = Alpha {
    alpha_alpha :: {-# UNPACK #-} !RSdouble,
    alpha_color :: !c }

instance (NFData c) => NFData (Alpha c) where
    rnf (Alpha a c) = rnf c

instance Functor Alpha where
    fmap f (Alpha a c) = Alpha a $ f c

instance (ImportColorCoordinates c) => ImportColorCoordinates (Alpha c) where
    importColorCoordinates f = Alpha 1.0 $ importColorCoordinates f

-- | Apply (more) transparency to a color.
alpha :: RSdouble -> Alpha c -> Alpha c
alpha x (Alpha a c) = Alpha (a*x) c

-- |  In the range \[0..255\].
alpha256 :: (Integral i) => i -> Alpha c -> Alpha c
alpha256 i = alpha (i2f256 i)

instance (AbstractZero c) => AbstractZero (Alpha c) where
    zero = Alpha 0 zero

instance (AbstractAdd c c) => AbstractAdd (Alpha c) (Alpha c) where
    add (Alpha a1 c1) (Alpha a2 c2) = Alpha (a1 + a2) (c1 `add` c2)

instance (AbstractSubtract c c) => AbstractSubtract (Alpha c) (Alpha c) where
    sub (Alpha a1 c1) (Alpha a2 c2) = Alpha (a1 - a2) (c1 `sub` c2)

instance (AbstractScale c) => AbstractScale (Alpha c) where
    scalarMultiply d (Alpha a c) = Alpha (d*a) $ scalarMultiply d c

instance (AbstractVector c) => AbstractVector (Alpha c)

