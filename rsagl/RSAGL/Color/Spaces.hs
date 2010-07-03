module RSAGL.Color.Spaces
    (color_space_cmy,
     color_wheel_rgb_brightness,
     color_wheel_cmyk)
    where

import RSAGL.Color.ColorSpace
import RSAGL.Color.RGB
import RSAGL.Math.Angle

-- | The subtractive Cyan-Magenta-Yellow color space.
color_space_cmy :: AffineColorSpace
color_space_cmy = newColorSpace (RGB 1 1 1)
                                (RGB 0 1 1)
                                (RGB 1 0 1)
                                (RGB 1 1 0)

-- | A color wheel using a simple definition of brightness,
-- @(red + green + blue) / 3@.  The hue definition is
-- identical to 'color_wheel_rgbl'.
color_wheel_rgb_brightness :: ColorWheel
color_wheel_rgb_brightness =
    newColorWheel (RGB 0 0 0)
                  (RGB 1 0 0,fromDegrees 0,1/3)
                  (RGB 0 1 0,fromDegrees 120,1/3)
                  (RGB 0 0 1,fromDegrees 240,1/3)

-- | A color wheel in the subtractive Cyan-Magenta-Yellow color space.
-- The hue definition is identical to 'color_wheel_rgbl'.
-- This color wheel uses a simple definition of black,
-- @(cyan + magenta + yellow) / 3@.
color_wheel_cmyk :: ColorWheel
color_wheel_cmyk =
    newColorWheel (RGB 1 1 1)
                  (RGB 0 1 1,fromDegrees 180,1/3)
                  (RGB 1 0 1,fromDegrees 300,1/3)
                  (RGB 1 1 0,fromDegrees 60 ,1/3)

