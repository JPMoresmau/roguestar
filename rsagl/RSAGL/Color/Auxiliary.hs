module RSAGL.Color.Auxiliary
    (i2f256)
    where

import RSAGL.Types

-- | Convert a value in the range \[0..255\] to the range
-- \[0.0..1.0\].
i2f256 :: (Integral i) => i -> RSdouble
i2f256 = (/ 255) . fromIntegral

