{-# LANGUAGE OverloadedStrings #-}
module Facing
    (Facing(..),
     facingToRelative,
     facingToRelative7,
     stringToFacing,
     facingDistance,
     isFacing,
     faceAt)
    where

import Position
import Data.Ord
import Data.List
import qualified Data.ByteString.Char8 as B

data Facing = North
	    | NorthEast
	    | East
	    | SouthEast
	    | South
	    | SouthWest
	    | West
	    | NorthWest
	    | Here
	      deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- |
-- Takes an abbreviation (n,e,sw, etc) and answers a facing.
-- The input string must be lower case.
-- No form of "Here" is an acceptable input to this function.
--
stringToFacing :: B.ByteString -> Maybe Facing
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

-- |
-- A test function to detect when one Position + Facing points directly at another Position.
--
isFacing :: (PositionType a,PositionType b) => (a, Facing) -> b -> Bool
isFacing (as,face) bs = or $ map (\(a,b) -> f face (fromPosition a) (fromPosition b)) $ positionPairs as bs
    where f :: Facing -> (Integer,Integer) -> (Integer,Integer) -> Bool
          f North (x,y) (u,v) = x == u && v >= y
          f NorthEast (x,y) (u,v) = x - u == y - v && u >= x
          f East (x,y) (u,v) = y == v && u >= x
          f SouthEast (x,y) (u,v) = x - u == v - y && u >= x
          f South (x,y) (u,v) = x == u && v <= y
          f SouthWest (x,y) (u,v) = x - u == y - v && u <= x
          f West (x,y) (u,v) = y == v && u <= x
          f NorthWest (x,y) (u,v) = x - y == v - y && u <= x
          f Here xy uv = xy == uv

-- |
-- Which facing most closely points from the first Position to the second.
--
faceAt :: Position -> Position -> Facing
faceAt here there = fst $ minimumBy (comparing snd) $
    map (\x -> let face = Position $ facingToRelative7 x in
                      (x,distanceBetweenSquared there face -
                         distanceBetweenSquared here face)) [minBound..maxBound]
