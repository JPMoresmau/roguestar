
module Quality
    (Quality(..),
     qualityToVertices)
    where


data Quality = Bad
	     | Poor
	     | Good
	     | Super
	     deriving (Eq,Enum,Ord,Show)

qualityToVertices :: Quality -> Integer
qualityToVertices Bad = 2^5
qualityToVertices Poor = 2^9
qualityToVertices Good = 2^13
qualityToVertices Super = 2^17

