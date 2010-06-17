
module Quality
    (Quality(..),
     qualityToVertices,
     qualityToFixed,
     minorFixedQuality)
    where

import RSAGL.Modeling

data Quality = Bad
	     | Poor
	     | Good
	     | Super
	     deriving (Eq,Enum,Ord,Show)

qualityToVertices :: Quality -> Integer
qualityToVertices Bad = 2^6
qualityToVertices Poor = 2^8
qualityToVertices Good = 2^10
qualityToVertices Super = 2^12

-- | Sets the fixed quality of of modeled surface using 'RSAGL.Model.fixed'.
-- Good for pieces that need to mesh together but don't for some reason.
qualityToFixed :: Quality -> ModelingM attr ()
qualityToFixed Bad = fixed (2^2,2^2)
qualityToFixed Poor = fixed (2^3,2^3)
qualityToFixed Good = fixed (2^4,2^4)
qualityToFixed Super = fixed (2^5,2^5)

-- | Sets the fixed quality of of modeled surface using 'RSAGL.Model.fixed'.
-- The quality here is very low.
minorFixedQuality :: Quality -> ModelingM attr ()
minorFixedQuality Bad = fixed (2^1,2^1)
minorFixedQuality Poor = fixed (2^1,2^1)
minorFixedQuality Good = fixed (2^2,2^2)
minorFixedQuality Super = fixed (2^3,2^3)
