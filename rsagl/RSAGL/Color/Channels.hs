module RSAGL.Color.Channels
    (channel_red,
     channel_green,
     channel_blue,
     channel_luminance,
     channel_cyan,
     channel_magenta,
     channel_yellow)
    where

import RSAGL.Color.ColorSpace
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

-- | Luminance of a channel as percieved by the human eye.
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

