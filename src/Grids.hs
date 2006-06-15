module Grids
    where

import RandomUtils

data Grid a = CompletelyRandomGrid Integer [(Integer,a)]
            | InterpolatedGrid Integer (Map (a,a) [(Integer,a)]) Grid
            | ArbitraryReplacementGrid Integer (Rational Integer) a [(Integer,a)] Grid
            | SpecificPlacementGrid (Map (Integer,Integer) a) Grid

gridAt :: Grid a -> (Integer,Integer) -> a
gridAt (CompletelyRandomGrid seed weights) at = weightedPick (randomIntegerGrid seed at) weights
gridAt (InterpolatedGrid seed interpolation_map grid) at@(x,y) = 
    let here = gridAt grid (x `div` 2,y `div` 2)
	there = gridAt grid (x `div` 2 + 1,y `div` 2 + 1)
	there_x = gridAt grid (x `div` 2 + 1,y `div` 2)
	there_y = gridAt grid (x `div` 2,y `div` 2 + 1)
	interpolate a1 a2 = weightedPick (randomIntegerGrid seed at) (interpolation_map ! at)
	in case (even x,even y) of
				(true,true) -> here
				(true,false) -> (interpolate here there_y)
				(false,true) -> (interpolate here there_x)
				(false,false) -> (interpolate here there)

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
generateGrid weights interps 0 seeds = 
    CompletelyRandomGrid (head seeds) weights
generateGrid weights interps n seeds = 
    InterpolatedGrid (head seeds) interps $ generateGrid weights interps (n-1) (tail seeds)
