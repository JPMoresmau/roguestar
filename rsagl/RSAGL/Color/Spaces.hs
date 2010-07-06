module RSAGL.Color.Spaces
    (color_space_neutral,
     color_space_cmy,
     color_wheel_rgb_brightness,
     color_wheel_cmyk,
     color_wheel_red_cyan_iso,
     color_wheel_blue_yellow_iso,
     color_wheel_green_magenta_iso)
    where

import RSAGL.Color.ColorSpace
import RSAGL.Color.RGB
import RSAGL.Math.Angle

-- | The RGB color space with neutral gray as the
-- origin.
color_space_neutral :: AffineColorSpace
color_space_neutral = newColorSpace (RGB 0.5 0.5 0.5)
                                    (RGB 1.0 0.5 0.5)
                                    (RGB 0.5 1.0 0.5)
                                    (RGB 0.5 0.5 1.0)

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

-- | A color wheel with an isotropic Red-Cyan channel.
-- The hue definition is identical to 'color_wheel_rgbl'.
color_wheel_red_cyan_iso :: ColorWheel
color_wheel_red_cyan_iso =
    newColorWheel (RGB 0 0 0)
                  (RGB 1 0 0,fromDegrees 0,1.0)
                  (RGB 0 1 0,fromDegrees 120,0.5)
                  (RGB 0 0 1,fromDegrees 240,0.5)

-- | A color wheel with an isotropic Blue-Yellow channel.
-- The hue definition is identical to 'color_wheel_rgbl'.
color_wheel_blue_yellow_iso :: ColorWheel
color_wheel_blue_yellow_iso =
    newColorWheel (RGB 0 0 0)
                  (RGB 1 0 0,fromDegrees 0,0.5)
                  (RGB 0 1 0,fromDegrees 120,0.5)
                  (RGB 0 0 1,fromDegrees 240,1.0)

-- | A color wheel with an isotropic Green-Magenta channel.
-- The hue definition is identical to 'color_wheel_rgbl'.
color_wheel_green_magenta_iso :: ColorWheel
color_wheel_green_magenta_iso =
    newColorWheel (RGB 0 0 0)
                  (RGB 1 0 0,fromDegrees 0,0.5)
                  (RGB 0 1 0,fromDegrees 120,1.0)
                  (RGB 0 0 1,fromDegrees 240,0.5)

