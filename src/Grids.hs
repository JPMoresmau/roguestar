module Grids
    where

import RandomUtils

data Grid a = CompletelyRandomGrid Integer [(Integer,a)]
            | InterpolatedGrid Integer (Map (a,a) [(Integer,a)]) Grid
            | ArbitraryReplacementGrid Integer (Rational Integer) a [(Integer,a)] Grid
            | PlacementGrid (Map (Integer,Integer) a) Grid

gridAt :: Grid a -> (Integer,Integer) -> a
gridAt (CompletelyRandomGrid seed weights) at = weightedPick (randomIntegerGrid seed at) weights
gridAt (InterpolatedGrid seed interpolation_map grid) at@(x,y) = 
    case (even x,even y) of
                         (true,true) -> gridAt (x `quot` 2,y `quot` 2) grid
                         (true,false) -> gridAt (x `quot` 2, )