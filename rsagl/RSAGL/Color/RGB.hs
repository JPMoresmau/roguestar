{-# LANGUAGE MultiParamTypeClasses #-}
module RSAGL.Color.RGB
    (RGB(..),
     rgb,
     rgb256,
     grayscale,
     greyscale,
     grayscale256,
     greyscale256,
     mapRGB,
     zipRGB,
     zipRGB3)
    where

import RSAGL.Types
import RSAGL.Color.ColorSpace
import RSAGL.Color.Auxiliary
import RSAGL.Math.Vector
import RSAGL.Math.AbstractVector
import Control.Parallel.Strategies

-- | A color in the red-green-blue color space.
data RGB = RGB { rgb_red   :: {-# UNPACK #-} !RSdouble,
                 rgb_green :: {-# UNPACK #-} !RSdouble,
                 rgb_blue  :: {-# UNPACK #-} !RSdouble }
    deriving (Eq,Ord,Read,Show)

instance NFData RGB where

instance ColorSpace RGB where
    affineColorSpaceOf _ = color_space_rgb

instance ExportColorCoordinates RGB where
    exportColorCoordinates (RGB r g b) =
        transformColorFromTo color_space_rgb $ Point3D r g b
instance ImportColorCoordinates RGB where
    importColorCoordinates f = RGB r g b
        where Point3D r g b = f color_space_rgb

-- | Construct a color from an RGB triple in the range \[0.0..1.0\].
rgb :: (ImportColorCoordinates c) => RSdouble -> RSdouble -> RSdouble -> c
rgb r g b = transformColor $ RGB r g b
{-# SPECIALIZE rgb :: RSdouble -> RSdouble -> RSdouble -> RGB #-}

-- | Construct a color from an RGB triple in the range \[0..255\].
rgb256 :: (ImportColorCoordinates c,Integral i) => i -> i -> i -> c
rgb256 r g b = rgb (i2f256 r) (i2f256 g) (i2f256 b)
{-# SPECIALIZE rgb256 :: (Integral i) => i -> i -> i -> RGB #-}

-- | Construct a gray color from a value in the range
-- \[0.0..1.0\].
grayscale :: (ImportColorCoordinates c) => RSdouble -> c
grayscale = greyscale
{-# SPECIALIZE grayscale :: RSdouble -> RGB #-}

greyscale :: (ImportColorCoordinates c) => RSdouble -> c
greyscale x = transformColor $ RGB x x x
{-# SPECIALIZE greyscale :: RSdouble -> RGB #-}

-- | Construct a gray color from a value in the range
-- \[0..255\].
grayscale256 :: (Integral i,ImportColorCoordinates c) => i -> c
grayscale256 = greyscale256
{-# SPECIALIZE grayscale256 :: (Integral i) => i -> RGB #-}

greyscale256 :: (Integral i,ImportColorCoordinates c) => i -> c
greyscale256 x = transformColor (rgb256 x x x :: RGB)
{-# SPECIALIZE greyscale256 :: (Integral i) => i -> RGB #-}

instance AbstractZero RGB where
    zero = RGB 0 0 0

instance AbstractAdd RGB RGB where
    add = zipRGB (+)

instance AbstractSubtract RGB RGB where
    sub = zipRGB (-)

instance AbstractScale RGB where
    scalarMultiply d = mapRGB (*d)

instance AbstractVector RGB

mapRGB :: (RSdouble -> RSdouble) -> RGB -> RGB
mapRGB f (RGB r g b) = RGB (f r) (f g) (f b)

zipRGB :: (RSdouble -> RSdouble -> RSdouble) -> RGB -> RGB -> RGB
zipRGB f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)

-- | A combining function on three RGB values.
zipRGB3 :: (RSdouble -> RSdouble -> RSdouble -> RSdouble) ->
           RGB -> RGB -> RGB -> RGB
zipRGB3 f (RGB r1 g1 b1) (RGB r2 g2 b2) (RGB r3 g3 b3) =
    RGB (f r1 r2 r3) (f g1 g2 g3) (f b1 b2 b3)

