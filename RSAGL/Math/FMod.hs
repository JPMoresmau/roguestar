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

instance FMod GLfloat where
    fmod n d = n - fromInteger f * d
        where f = floor $ n / d

instance FMod GLdouble where
    fmod n d = n - fromInteger f * d
        where f = floor $ n / d
