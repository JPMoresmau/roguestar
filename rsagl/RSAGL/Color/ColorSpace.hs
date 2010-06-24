module RSAGL.Color.ColorSpace
    (ColorSpace(..),
     Color(..),
     AffineColorSpace,
     ColorWheel,
     color_space_rgb,
     color_wheel_rgbl,
     transformColorToFrom,
     transformColor)
    where

import RSAGL.Types
import RSAGL.Math.Matrix

-- | An affine transformation of the default RGB color space.
newtype AffineColorSpace = AffineColorSpace Matrix

-- | A rotatable color space.
newtype ColorWheel = ColorWheel Matrix

class ColorSpace cs where
    affineColorSpaceOf :: cs -> AffineColorSpace

class (ColorSpace c) => Color c where
    toColorCoordinates :: c -> (RSdouble,RSdouble,RSdouble)
    fromColorCoordinates :: (RSdouble,RSdouble,RSdouble) -> c

instance ColorSpace AffineColorSpace where
    affineColorSpaceOf = id

instance ColorSpace ColorWheel where
    affineColorSpaceOf (ColorWheel m) = AffineColorSpace m

-- | A color wheel constructed with red, green and blue device primaries
-- and a luminance component.  This is the basis of the HCL color system.
{-# NOINLINE color_wheel_rgbl #-}
color_wheel_rgbl :: ColorWheel
color_wheel_rgbl = ColorWheel $ matrixInverse $ matrix $
  [ [1.0    , -0.5       , -0.5                  , 0.0],
    [0.0    , (sqrt 3/2) , (negate $ sqrt 3 / 2) , 0.0],
    [0.2126 , 0.7152     , 0.0722                , 0.0],
    [0.0    , 0.0        , 0.0                   , 1.0] ]

-- | The red-green-blue device color space.
{-# NOINLINE color_space_rgb #-}
color_space_rgb :: AffineColorSpace
color_space_rgb = AffineColorSpace $ identity_matrix

-- | Transform order triples between color spaces.
transformColorToFrom :: AffineColorSpace ->
                        AffineColorSpace ->
                        (RSdouble,RSdouble,RSdouble) ->
                        (RSdouble,RSdouble,RSdouble)
transformColorToFrom (AffineColorSpace destination)
                     (AffineColorSpace source)
                     (u,v,w) =
    transformHomogenous u v w 1.0
                        (\u' v' w' -> (u',v',w'))
                        (matrixInverse destination `matrixMultiply` source)

transformColor :: (Color source,Color dest) => source -> dest
transformColor source = result
    where triple = transformColorToFrom (affineColorSpaceOf result)
                                        (affineColorSpaceOf source)
                                        (toColorCoordinates source)
          result = fromColorCoordinates triple

