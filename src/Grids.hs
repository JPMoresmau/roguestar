module Grids
    (Grid,
     gridAt,
     generateGrid)
    where

import RNG
import RandomUtils
import Data.Map as Map
import Data.Ratio

data Grid a = CompletelyRandomGrid Integer [(Integer,a)]
            | InterpolatedGrid Integer (Map (a,a) [(Integer,a)]) (Grid a)
            | ArbitraryReplacementGrid Integer (Rational) a [(Integer,a)] (Grid a)
            | SpecificPlacementGrid (Map (Integer,Integer) a) (Grid a)

gridAt :: Ord a => Grid a -> (Integer,Integer) -> a
gridAt (CompletelyRandomGrid seed weights) at = weightedPick (randomIntegerGrid seed at) weights
gridAt (InterpolatedGrid seed interpolation_map grid) at@(x,y) = 
    let here = gridAt grid (x `div` 2,y `div` 2)
	there = gridAt grid (x `div` 2 + 1,y `div` 2 + 1)
	there_x = gridAt grid (x `div` 2 + 1,y `div` 2)
	there_y = gridAt grid (x `div` 2,y `div` 2 + 1)
	interpolate a1 a2 = weightedPick (randomIntegerGrid seed at) (interpolation_map ! (a1,a2))
	in case (even x,even y) of
				(True,True) -> here
				(True,False) -> (interpolate here there_y)
				(False,True) -> (interpolate here there_x)
				(False,False) -> (interpolate here there)

gridAt (ArbitraryReplacementGrid seed frequency rep_val weights grid) at =
    let here = gridAt grid at
	in 
	if here == rep_val && 
	       ((randomIntegerGrid seed at) `mod` (denominator frequency)
		<
		(numerator frequency))
	then weightedPick (randomIntegerGrid seed at) weights
	else here

gridAt (SpecificPlacementGrid rep_map grid) at =
    findWithDefault (gridAt grid at) at rep_map

-- |
-- Generates a random grid.  The first Integer, smoothness,
-- indicates the recursion depth for the generator.  The
-- Integer list is the random integer stream used to generate
-- the map.
generateGrid :: [(Integer,a)] -> Map (a,a) [(Integer,a)] -> Integer -> [Integer] -> Grid a
generateGrid weights _ 0 seeds = 
    CompletelyRandomGrid (head seeds) weights
generateGrid weights interps n seeds = 
    InterpolatedGrid (head seeds) interps $ generateGrid weights interps (n-1) (tail seeds)

-- |
-- Replaces a random portion of a certain element on a grid
-- with a member of a random selection of elements.
--

-- arbitraryGridReplace