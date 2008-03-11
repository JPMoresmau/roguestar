
module Facing
    (Facing(..),
     facingToRelative,
     facingToRelative7,
     stringToFacing,
     facingDistance)
    where

data Facing = North
	    | NorthEast
	    | East
	    | SouthEast
	    | South
	    | SouthWest
	    | West
	    | NorthWest
	    | Here
	      deriving (Eq,Enum,Bounded,Read,Show)

-- |
-- Takes an abbreviation (n,e,sw, etc) and answers a facing.
-- The input string must be lower case.
-- No form of "Here" is an acceptable input to this function.
--
stringToFacing :: String -> Maybe Facing
stringToFacing "n" = Just North
stringToFacing "ne" = Just NorthEast
stringToFacing "e" = Just East
stringToFacing "se" = Just SouthEast
stringToFacing "s" = Just South
stringToFacing "sw" = Just SouthWest
stringToFacing "w" = Just West
stringToFacing "nw" = Just NorthWest
stringToFacing _ = Nothing

-- |
-- In relative coordinates, one integral step in the specified direction.
--
facingToRelative :: Facing -> (Integer,Integer)
facingToRelative North = (0,1)
facingToRelative NorthEast = (1,1)
facingToRelative East = (1,0)
facingToRelative SouthEast = (1,-1)
facingToRelative South = (0,-1)
facingToRelative SouthWest = (-1,-1)
facingToRelative West = (-1,0)
facingToRelative NorthWest = (-1,1)
facingToRelative Here = (0,0)

-- |
-- In relative coordinates, roughly seven integral steps in the specified direction.
-- Note: 7 is a small integer such that the square root of 2*5^2 is quite close to 7.
--
facingToRelative7 :: Facing -> (Integer,Integer)
facingToRelative7 North = (0,7)
facingToRelative7 NorthEast = (5,5)
facingToRelative7 East = (7,0)
facingToRelative7 SouthEast = (5,-5)
facingToRelative7 South = (0,-7)
facingToRelative7 SouthWest = (-5,-5)
facingToRelative7 West = (-7,0)
facingToRelative7 NorthWest = (-5,5)
facingToRelative7 Here = (0,0)

-- |
-- The distance between two facings, between 0 and 4.
--
facingDistance :: Facing -> Facing -> Integer
facingDistance Here _ = 0
facingDistance _ Here = 0
facingDistance a b = toInteger $ if enum_distance > 4 then 8 - enum_distance else enum_distance
    where enum_distance = abs $ fromEnum a - fromEnum b
