module RSAGL.Color.Channels
    (channel_red,
     channel_green,
     channel_blue,
     channel_luminance,
     channel_chroma,
     channel_cyan,
     channel_magenta,
     channel_yellow,
     channel_brightness,
     channel_red_cyan,
     channel_blue_yellow,
     channel_green_magenta,
     channel_value,
     channel_boldness,
     channel_intensity)
    where

import RSAGL.Color.ColorSpace
import RSAGL.Math.Angle
import RSAGL.Color.Spaces

-- | The red channel of the RGB color space.
channel_red :: ColorChannel
channel_red = newChannel channel_u color_space_rgb

-- | The green channel of the RGB color space.
channel_green :: ColorChannel
channel_green = newChannel channel_v color_space_rgb

-- | The blue channel of the RGB color space.
channel_blue :: ColorChannel
channel_blue = newChannel channel_w color_space_rgb

-- | The Chroma (colorfulness) of a color, isotropic to luminance.
channel_chroma :: ColorChannel
channel_chroma = newRadialChannel color_wheel_rgbl

-- | Luminance of a color as percieved by the human eye.
channel_luminance :: ColorChannel
channel_luminance = newChannel channel_w color_wheel_rgbl

-- | The cyan channel of the CMY color space.
channel_cyan :: ColorChannel
channel_cyan = newChannel channel_u color_space_cmy

-- | The magenta channel of the CMY color space.
channel_magenta :: ColorChannel
channel_magenta = newChannel channel_v color_space_cmy

-- | The yellow channel of the CMY color space.
channel_yellow :: ColorChannel
channel_yellow = newChannel channel_w color_space_cmy

-- | The brightness channel in simple device interpretation.
-- That is, (red + green + blue) / 3.
channel_brightness :: ColorChannel
channel_brightness = newChannel channel_w color_wheel_rgb_brightness

-- | A red (1.0) vs. cyan (-1.0) channel.
channel_red_cyan :: ColorChannel
channel_red_cyan = newAngularChannel color_wheel_red_cyan_iso (fromDegrees 0)

-- | A blue (1.0) vs. yellow (-1.0) channel.
channel_blue_yellow :: ColorChannel
channel_blue_yellow = newAngularChannel color_wheel_blue_yellow_iso (fromDegrees 240)

-- | A green (1.0) vs. magenta (-1.0) channel.
channel_green_magenta :: ColorChannel
channel_green_magenta = newAngularChannel color_wheel_green_magenta_iso (fromDegrees 120)

-- | The maximum channel of the additive RGB color space.
-- This is identical to the value channel of the HSB color model.
-- This channel represents the device gamut: it should be between 0 and 1.
channel_value :: ColorChannel
channel_value = newMaximalChannel color_space_rgb

-- | The maximum channel of the subtractive CMY color space.
-- This channel represents the device gamut: it should be between 0 and 1.
channel_boldness :: ColorChannel
channel_boldness = newMaximalChannel color_space_cmy

-- | The maximum channel of the neutral RGB color space.
-- This channel represents the device gamut: it should be between -1 and 1.
channel_intensity :: ColorChannel
channel_intensity = newMaximalChannel color_space_neutral

