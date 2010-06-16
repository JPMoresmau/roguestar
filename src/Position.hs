module Position
    (Position(..),
     MultiPosition,
     multiPosition,
     PositionType(..),
     distanceBetweenSquared,
     distanceBetweenChessboard,
     positionPairs)
    where

import Data.List
import qualified Data.Set as Set

-- | Position of an object in \"chessboard space\".
newtype Position = Position { fromPosition :: (Integer,Integer) }
    deriving (Eq,Ord,Read,Show)

-- | For objects, such as buildings, that occupy multiple positions.
newtype MultiPosition = MultiPosition { fromMultiPosition :: [Position] }

instance Eq MultiPosition where
    (==) (MultiPosition as) (MultiPosition bs) = Set.fromList as == Set.fromList bs

instance Ord MultiPosition where
    compare (MultiPosition as) (MultiPosition bs) = Set.fromList as `compare` Set.fromList bs

class PositionType p where
    toMultiPosition :: p -> MultiPosition
    offsetPosition :: (Integer,Integer) -> p -> p

instance PositionType Position where
    toMultiPosition p = MultiPosition [p]
    offsetPosition (x,y) (Position (u,v)) = Position (x+u,y+v)

instance PositionType MultiPosition where
    toMultiPosition = id
    offsetPosition xy (MultiPosition ps) = MultiPosition $ map (offsetPosition xy) ps

-- | Construct a 'MultiPosition' from a base position and a list of offsets.
-- The base position always counts as part of the MultiPosition.
multiPosition :: Position -> [(Integer,Integer)] -> MultiPosition
multiPosition (Position xy) xys = MultiPosition $ nub $ Position xy : map (offsetPosition xy . Position) xys

-- | Pythagorean distance, squared.
-- For multi-positions, measures the minimal distance.
distanceBetweenSquared :: (PositionType a,PositionType b) => a -> b -> Integer
distanceBetweenSquared as bs = minimum $
    do Position (x,y) <- fromMultiPosition $ toMultiPosition as
       Position (u,v) <- fromMultiPosition $ toMultiPosition bs
       return $ (x - u)^2 + (y - v)^2

-- | Number of squares you would have to move (as a queen on a chessboard) to arrive from the first position to the second.
-- For multi-positions, measures the minimal distance.
distanceBetweenChessboard :: (PositionType a,PositionType b) => a -> b -> Integer
distanceBetweenChessboard as bs = minimum $
    do Position (x,y) <- fromMultiPosition $ toMultiPosition as
       Position (u,v) <- fromMultiPosition $ toMultiPosition bs
       return $ max (abs $ u - x) (abs $ v - y)

-- | List all pairs of positions between two MutiPositions.
positionPairs :: (PositionType a,PositionType b) => a -> b -> [(Position,Position)]
positionPairs as bs = 
    do a <- fromMultiPosition $ toMultiPosition as
       b <- fromMultiPosition $ toMultiPosition bs
       return (a,b)
