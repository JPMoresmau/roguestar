module RSAGL.Color.ColorSpace
    (ColorSpace(..),
     ImportColorCoordinates(..),
     ExportColorCoordinates(..),
     AffineColorSpace,
     ColorWheel,
     ColorChannel,
     ChannelIndex,
     LinearMetric(..),
     newChannel,
     viewChannel,
     channel_u,
     channel_v,
     channel_w,
     color_space_rgb,
     color_wheel_rgbl,
     transformColorFromTo,
     transformColor)
    where

import RSAGL.Types
import RSAGL.Math.Matrix
import Data.Vec (nearZero)

-- | An affine transformation of the default RGB color space.
newtype AffineColorSpace = AffineColorSpace Matrix
    deriving (Show)

-- | A rotatable color space.
newtype ColorWheel = ColorWheel Matrix
    deriving (Show)

-- | A specific component of a 3-channel color space.
newtype ColorChannel = LinearChannel Matrix
    deriving (Show)

-- | 'channel_u', 'channel_v', 'channel_w', of a 3-channel color space.
data ChannelIndex = ChannelIndex Matrix

-- | The first channel of a color space represented by the ordered tripple,
-- @(u,v,w)@.
channel_u :: ChannelIndex
channel_u = ChannelIndex identity_matrix

-- | The second channel of a color space represented by the ordered tripple,
-- @(u,v,w)@.
channel_v :: ChannelIndex
channel_v = ChannelIndex $ matrix
    [[0,1,0,0],
     [1,0,0,0],
     [0,0,1,0],
     [0,0,0,1]]

-- | The third channel of a color space represented by the ordered tripple,
-- @(u,v,w)@.
channel_w :: ChannelIndex
channel_w = ChannelIndex $ matrix
   [[0,0,1,0],
    [0,1,0,0],
    [1,0,0,0],
    [0,0,0,1]]

-- | Pick a channel from a color space.
newChannel :: (ColorSpace cs) => ChannelIndex -> cs -> ColorChannel
newChannel (ChannelIndex ch_ix) cs = LinearChannel $ m `matrixMultiply` ch_ix
    where (AffineColorSpace m) = affineColorSpaceOf cs

-- | A view of a specific color channel, such as red, or luminance.
data LinearMetric = LinearMetric {
    -- | The range of a color channel that is within gamut.
    -- This range depends on the channel and the particular
    -- color being observed, and may not exist if the
    -- color itself is out of gamut.
    linear_gamut_bounds :: Maybe (RSdouble,RSdouble),
    -- | A function to modify a color channel independantly
    -- from the other color channels in the same color space.
    linear_color_function :: RSdouble -> Color,
    -- | The value of the particular color channel for the
    -- particular color.
    linear_value :: RSdouble }

instance Show LinearMetric where
    show x = show (linear_gamut_bounds x, linear_value x)

-- | Read a specific channel of a color.
viewChannel :: (ExportColorCoordinates c) =>
                  ColorChannel -> c -> LinearMetric
viewChannel (LinearChannel m) c = LinearMetric {
              linear_gamut_bounds = case intersections of
                  [] -> Nothing
                  is -> Just (minimum is,maximum is),
              linear_color_function = \x -> importColorCoordinates $
                  transformColorFromTo color_space_rgb (x,v,w),
              linear_value = u }
    where (u,v,w) = exportColorCoordinates c $ AffineColorSpace m
          (r,g,b) = exportColorCoordinates c color_space_rgb
          (r',g',b') = colorVectorOf (AffineColorSpace m) (1,0,0)
          intersections = map (+u) $ filter gamutValid $
               map (uncurry (/)) $ filter (not . nearZero . snd)
                   [(1-r,r'),(1-g,g'),(1-b,b'),
                    (-r,r'),(-g,g'),(-b,b')]
          colorFunction x = (r+r'*x,g+g'*x,b+b'*x)
          gamutValid x = let (r'',g'',b'') = colorFunction x
              in (>=3) $ length $ filter (>= (-0.001)) $ filter (<= 1.001) $ [r'',g'',b'']

-- | A color space specification or color type that has an associated
-- color space.
--
-- If a type implements both 'ImportColorCoordinates' and
-- 'ColorSpace', then it must ensure that:
--
-- @importColorCoordinates f =
--     (let c = importColorCoordinates (const $ f $ affineColorSpaceOf c) in c)@
--
-- Note that the color space is defined using value recursion.
--
class ColorSpace cs where
    affineColorSpaceOf :: cs -> AffineColorSpace

-- | A color type that can export its color coordinates.
-- An easy implementation is
--
-- @transformColorFromTo your_color_space your_color_coordinates@
--
-- If a type implements both 'ExportColorCoordinates' and
-- 'ImportColorCoordinates', then it must ensure that
-- @importColorCoordinates . exportColorCoordinates = id@.
--
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

-- | A generic representation of Color.
newtype Color = Color { fromColor ::
    AffineColorSpace -> (RSdouble,RSdouble,RSdouble) }

instance ExportColorCoordinates Color where
    exportColorCoordinates = fromColor

instance ImportColorCoordinates Color where
    importColorCoordinates = Color

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

-- | Transform a color vector into RGB space.
colorVectorOf :: AffineColorSpace ->
                 (RSdouble,RSdouble,RSdouble) ->
                 (RSdouble,RSdouble,RSdouble)
colorVectorOf (AffineColorSpace m) (u,v,w) =
    transformHomogenous u v w 0.0
                        (\u' v' w' -> (u',v',w'))
                        m

{-# RULES
"transformColor::a->a"    transformColor = id
  #-}
-- | Transform colors between color spaces.
transformColor :: (ExportColorCoordinates source,
                   ImportColorCoordinates dest) =>
                  source -> dest
transformColor = importColorCoordinates . exportColorCoordinates

