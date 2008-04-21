module Position
    (Position(..),
     distanceBetweenSquared,
     offsetPosition)
    where

newtype Position = Position { fromPosition :: (Integer,Integer) }
    deriving (Eq,Ord,Read,Show)

offsetPosition :: (Integer,Integer) -> Position -> Position
offsetPosition (x,y) (Position (u,v)) = Position (x+u,y+v)

distanceBetweenSquared :: Position -> Position -> Integer
distanceBetweenSquared (Position (x,y)) (Position (u,v)) = (x - u)^2 + (y - v)^2
