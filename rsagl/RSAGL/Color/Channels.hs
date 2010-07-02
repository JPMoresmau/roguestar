module RSAGL.Color.Channels
    (channel_red,
     channel_green,
     channel_blue,
     channel_luminance)
    where

import RSAGL.Color.ColorSpace

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

