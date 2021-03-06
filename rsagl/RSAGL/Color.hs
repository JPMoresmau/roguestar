{-# LANGUAGE MultiParamTypeClasses #-}
module RSAGL.Color
    (module RSAGL.Color.RGB,
     module RSAGL.Color.Alpha,
     module RSAGL.Color.HCL,
     module RSAGL.Color.Spaces,
     module RSAGL.Color.Channels,
     module RSAGL.Color.LinearAdjust,
     module RSAGL.Color.ColorSpace,
     module RSAGL.Color.OpenGL,
     RGBA,
     rgba,
     minRGB,
     invertRGB,
     filterRGB,
     filterRGBLinear)
    where

import RSAGL.Color.RGB
import RSAGL.Color.Alpha
import RSAGL.Color.HCL
import RSAGL.Color.ColorSpace
import RSAGL.Color.Spaces
import RSAGL.Color.Channels
import RSAGL.Color.LinearAdjust
import RSAGL.Color.OpenGL
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL.Raw.Core31 (GLdouble,GLfloat)
import Graphics.Rendering.OpenGL hiding (RGB,RGBA,Alpha)
import RSAGL.Math.AbstractVector
import RSAGL.Math.Interpolation
import RSAGL.Math.Types

type RGBA = Alpha RGB

-- | Construct an RGBA color.
rgba :: RSdouble -> RSdouble -> RSdouble -> RSdouble -> RGBA
rgba r g b a = Alpha a $ RGB r g b

-- | maps an RGB color between a black point and a white point.
-- The first parameter, the black point, will map to RGB 0 0 0.
-- The second parameter, the white point, will map to RGB 1 1 1.
filterRGBLinear :: RGB -> RGB -> RGB -> RGB
filterRGBLinear black_point white_point = zipRGB3
    (\b w c -> lerpBetweenClamped (b,c,w) (0,1)) black_point white_point

-- | Get the minimum of the three RGB components.
minRGB :: RGB -> RSdouble
minRGB (RGB r g b) = min r (min g b)

filterRGB :: RGB -> RGB -> RGB
filterRGB = zipRGB (*)

-- | Arithmetic inverse of a color.
invertRGB :: RGB -> RGB
invertRGB = mapRGB (1-)

---------------------------------------------------------------------------

