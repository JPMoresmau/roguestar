module RSAGL.Color.Spaces
    (color_space_cmy)
    where

import RSAGL.Color.ColorSpace
import RSAGL.Color.RGB

-- | The subtractive Cyan-Magenta-Yellow color space.
color_space_cmy :: AffineColorSpace
color_space_cmy = newColorSpace (RGB 1 1 1)
                                (RGB 0 1 1)
                                (RGB 1 0 1)
                                (RGB 1 1 0)

