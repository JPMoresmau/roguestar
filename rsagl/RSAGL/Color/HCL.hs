module RSAGL.Color.HCL
    (HCL(..))
    where

import RSAGL.Types
import RSAGL.Math.Angle
import RSAGL.Color.ColorSpace

-- | A color in the hue-chroma-luminance color space.
data HCL = HCL { hcl_hue        :: {-# UNPACK #-} !Angle,
                 hcl_chroma     :: {-# UNPACK #-} !RSdouble,
                 hcl_luminance  :: {-# UNPACK #-} !RSdouble }
    deriving (Eq,Ord,Read,Show)

instance ColorSpace HCL where
    affineColorSpaceOf _ = affineColorSpaceOf color_wheel_rgbl

instance Color HCL where
    toColorCoordinates (HCL h c l) = (u,v,l)
        where (u,v) = polarToCartesian (h,c)
    fromColorCoordinates (u,v,l) = HCL h c l
        where (h,c) = cartesianToPolar (u,v)

