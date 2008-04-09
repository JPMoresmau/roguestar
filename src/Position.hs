module Position
    (Position(..),
     distanceBetweenSquared)
    where

newtype Position = Position (Integer,Integer)
    deriving (Eq,Ord,Read,Show)

distanceBetweenSquared :: Position -> Position -> Integer
distanceBetweenSquared (Position (x,y)) (Position (u,v)) = (x - u)^2 + (y - v)^2
