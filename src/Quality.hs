
module Quality
    (Quality(..),
     qualityToVertices,
     qualityToFixed)
    where

import RSAGL.Model

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
qualityToFixed :: Quality -> ModelingM attr ()
qualityToFixed Bad = fixed (2^3,2^2)
qualityToFixed Poor = fixed (2^4,2^3)
qualityToFixed Good = fixed (2^5,2^4)
qualityToFixed Super = fixed (2^6,2^5)
