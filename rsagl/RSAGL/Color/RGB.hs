module RSAGL.Color.RGB
    (RGB(..))
    where

import RSAGL.Types
import RSAGL.Color.ColorSpace

-- | A color in the red-green-blue color space.
data RGB = RGB { rgb_red   :: {-# UNPACK #-} !RSdouble,
                 rgb_green :: {-# UNPACK #-} !RSdouble,
                 rgb_blue  :: {-# UNPACK #-} !RSdouble }
    deriving (Eq,Ord,Read,Show)

instance ColorSpace RGB where
    affineColorSpaceOf _ = color_space_rgb

instance Color RGB where
    toColorCoordinates (RGB r g b) = (r,g,b)
    fromColorCoordinates (r,g,b) = RGB r g b

