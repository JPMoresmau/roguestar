module RSAGL.Color.ColorSpace
    (ColorSpace(..),
     ImportColorCoordinates(..),
     ExportColorCoordinates(..),
     AffineColorSpace,
     ColorWheel,
     color_space_rgb,
     color_wheel_rgbl,
     transformColorFromTo,
     transformColor)
    where

import RSAGL.Types
import RSAGL.Math.Matrix

-- | An affine transformation of the default RGB color space.
newtype AffineColorSpace = AffineColorSpace Matrix

-- | A rotatable color space.
newtype ColorWheel = ColorWheel Matrix

-- | A color space specification or color type that has an associated
-- color space.
class ColorSpace cs where
    affineColorSpaceOf :: cs -> AffineColorSpace

-- | A color type that can export its color coordinates.
-- An easy implementation is
-- @transformColorFromTo your_color_space your_color_coordinates@
class ExportColorCoordinates c where
    exportColorCoordinates ::
        c -> AffineColorSpace -> (RSdouble,RSdouble,RSdouble)

-- | A color type that can import its color coordinates.
class ImportColorCoordinates c where
    importColorCoordinates ::
        (AffineColorSpace -> (RSdouble,RSdouble,RSdouble)) -> c

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

-- | Transform ordered triples between color spaces.
transformColorFromTo :: AffineColorSpace ->
                        (RSdouble,RSdouble,RSdouble) ->
                        AffineColorSpace ->
                        (RSdouble,RSdouble,RSdouble)
transformColorFromTo (AffineColorSpace source)
                     (u,v,w)
                     (AffineColorSpace destination) =
    transformHomogenous u v w 1.0
                        (\u' v' w' -> (u',v',w'))
                        (matrixInverse destination `matrixMultiply` source)

{-# RULES
"transformColor::a->a"    transformColor = id
  #-}
-- | Transform colors between color spaces.
transformColor :: (ExportColorCoordinates source,
                   ImportColorCoordinates dest) =>
                  source -> dest
transformColor = importColorCoordinates . exportColorCoordinates

