module RSAGL.Math.FMod
    (FMod(..))
    where

import RSAGL.Types

class FMod f where
    fmod :: f -> f -> f

instance FMod Float where
    fmod n d = n - fromInteger f * d
        where f = floor $ n / d

instance FMod Double where
    fmod n d = n - fromInteger f * d
        where f = floor $ n / d

instance FMod RSfloat where
    fmod n d = n - fromInteger f * d
        where f = floor $ n / d

instance FMod RSdouble where
    fmod n d = n - fromInteger f * d
        where f = floor $ n / d
