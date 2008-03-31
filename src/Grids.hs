
module Grids
    (Grid,
     gridAt,
     generateGrid,
     arbitraryReplaceGrid)
    where

import RNG
import RandomUtils
import ListUtils
import Data.Map as Map
import Data.Ratio
import Data.List

data Grid a = CompletelyRandomGrid Integer ((Integer,Integer) -> Integer) [(Integer,a)]
            | InterpolatedGrid Integer ((Integer,Integer) -> Integer) (Map (a,a) [(Integer,a)]) (Grid a)
            | ArbitraryReplacementGrid Integer ((Integer,Integer) -> Integer) [(Rational,a)] [(Integer,a)] (Grid a)
            | SpecificPlacementGrid (Map (Integer,Integer) a) (Grid a)
	    | CachedGrid ((Integer,Integer) -> a) (Grid a)

data Grid_Persistant a = CompletelyRandomGrid_Persistant Integer [(Integer,a)]
		       | InterpolatedGrid_Persistant Integer [((a,a),[(Integer,a)])] (Grid_Persistant a)
		       | ArbitraryReplacementGrid_Persistant Integer [(Rational,a)] [(Integer,a)] (Grid_Persistant a)
		       | SpecificPlacementGrid_Persistant [((Integer,Integer),a)] (Grid_Persistant a)
		       deriving (Read,Show)

toPersistant :: (Grid a) -> (Grid_Persistant a)
toPersistant (CompletelyRandomGrid x _ prob_list) = 
    CompletelyRandomGrid_Persistant x prob_list
toPersistant (InterpolatedGrid x _ prob_map grid) = 
    InterpolatedGrid_Persistant x (toList prob_map) (toPersistant grid)
toPersistant (ArbitraryReplacementGrid x _ sources replacements grid) = 
    ArbitraryReplacementGrid_Persistant x sources replacements $ toPersistant grid
toPersistant (SpecificPlacementGrid placement_map grid) = 
    SpecificPlacementGrid_Persistant (toList placement_map) (toPersistant grid)
toPersistant (CachedGrid _ grid) = toPersistant grid

fromPersistant :: (Ord a) => (Grid_Persistant a) -> (Grid a)
fromPersistant (CompletelyRandomGrid_Persistant x prob_list) = 
    cachedGridOf $ CompletelyRandomGrid x (randomIntegerGrid x) prob_list
fromPersistant (InterpolatedGrid_Persistant x prob_map grid) =
    cachedGridOf $ InterpolatedGrid x (randomIntegerGrid x) (fromList prob_map) (fromPersistant grid)
fromPersistant (ArbitraryReplacementGrid_Persistant x sources replacements grid) =
    cachedGridOf $ ArbitraryReplacementGrid x (randomIntegerGrid x) sources replacements (fromPersistant grid)
fromPersistant (SpecificPlacementGrid_Persistant placement_map grid) =
    cachedGridOf $ SpecificPlacementGrid (fromList placement_map) (fromPersistant grid)

fromPersistant_tupled :: (Ord a) => (Grid_Persistant a,String) -> (Grid a,String)
fromPersistant_tupled (x,y) = (fromPersistant x,y)

instance (Show a) => Show (Grid a) where
    show grid = show $ toPersistant grid

instance (Ord a, Read a) => Read (Grid a) where
    readsPrec n = \x -> Prelude.map fromPersistant_tupled (readsPrec n x)

gridAt :: Ord a => Grid a -> (Integer,Integer) -> a
gridAt (CompletelyRandomGrid _ seedfn weights) at = weightedPick (seedfn at) weights
gridAt (InterpolatedGrid _ seedfn interpolation_map grid) at@(x,y) = 
    let here = gridAt grid (x `div` 2,y `div` 2)
	there = gridAt grid (x `div` 2 + 1,y `div` 2 + 1)
	there_x = gridAt grid (x `div` 2 + 1,y `div` 2)
	there_y = gridAt grid (x `div` 2,y `div` 2 + 1)
	interpolate a1 a2 = weightedPick (seedfn at) (interpolation_map ! (a1,a2))
	in case (even x,even y) of
				(True,True) -> here
				(True,False) -> (interpolate here there_y)
				(False,True) -> (interpolate here there_x)
				(False,False) -> (interpolate here there)

gridAt (ArbitraryReplacementGrid _ seedfn sources replacements grid) at = 
    case fmap fst $ find ((== here) . snd) sources of
         Just frequency | ((seedfn at) `mod` (denominator frequency) < (numerator frequency)) ->
	     weightedPick (seedfn at) replacements
	 _ -> here
  where here = gridAt grid at

gridAt (SpecificPlacementGrid rep_map grid) at =
    findWithDefault (gridAt grid at) at rep_map

gridAt (CachedGrid map_fn _) at = map_fn at

cachedGridOf :: Ord a => Grid a -> Grid a
cachedGridOf already_cached_grid@(CachedGrid _ _) = already_cached_grid
cachedGridOf any_other_grid = CachedGrid (cachedAccessor2D (gridAt any_other_grid)) any_other_grid

-- |
-- Generates a random grid.  The first Integer, smoothness,
-- indicates the recursion depth for the generator.  The
-- Integer list is the random integer stream used to generate
-- the map.
generateGrid :: (Ord a) => [(Integer,a)] -> Map (a,a) [(Integer,a)] -> Integer -> [Integer] -> Grid a
generateGrid weights _ 0 seeds = let seed = head seeds
				      in CompletelyRandomGrid seed (randomIntegerGrid seed) weights
generateGrid weights interps n seeds = let seed = head seeds
					    in cachedGridOf $ InterpolatedGrid seed (randomIntegerGrid seed) interps $ 
					       generateGrid weights interps (n-1) (tail seeds)

-- |
-- Arbitrarily (randomly) replaces some elements of a grid with another.
--
arbitraryReplaceGrid :: (Ord a) => [(Rational,a)] -> [(Integer,a)] -> Integer -> Grid a -> Grid a
arbitraryReplaceGrid sources replacements seed grid = cachedGridOf $
    ArbitraryReplacementGrid seed (randomIntegerGrid seed) sources replacements grid
