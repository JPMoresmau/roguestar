{-# LANGUAGE MultiParamTypeClasses #-}
module RSAGL.Color.HCL
    (HCL(..))
    where

import RSAGL.Math.AbstractVector
import RSAGL.Types
import RSAGL.Math.Vector
import RSAGL.Math.Angle
import RSAGL.Color.ColorSpace

-- | A color in the hue-chroma-luminance color space.
-- This is an additive color system (like RGB).
data HCL = HCL { hcl_hue        :: {-# UNPACK #-} !Angle,
                 hcl_chroma     :: {-# UNPACK #-} !RSdouble,
                 hcl_luminance  :: {-# UNPACK #-} !RSdouble }
    deriving (Eq,Ord,Read,Show)

instance ColorSpace HCL where
    affineColorSpaceOf _ = affineColorSpaceOf color_wheel_rgbl

instance ExportColorCoordinates HCL where
    exportColorCoordinates (HCL h c l) =
        transformColorFromTo (affineColorSpaceOf color_wheel_rgbl) $ Point3D u v l
        where (u,v) = polarToCartesian (h,c)

instance ImportColorCoordinates HCL where
    importColorCoordinates f = HCL h c l
        where (h,c) = cartesianToPolar (u,v)
              Point3D u v l = f $ affineColorSpaceOf $ color_wheel_rgbl

