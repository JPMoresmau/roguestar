module Grids
    (Grid,
     gridAt,
     generateGrid,
     arbitraryReplaceGrid,
     specificReplaceGrid)
    where

import RNG
import Data.Map as Map
import Data.Ratio
import Data.List as List
import Random
import Data.MemoCombinators
import Control.Arrow

newtype SeededGrid = SeededGrid Integer deriving (Read,Show)
data StorableCachedGrid a = StorableCachedGrid (Grid a) ((Integer,Integer) -> a)

instance (Show a) => Show (StorableCachedGrid a) where
    show (StorableCachedGrid g _) = show g

instance (Read a,Ord a) => Read (StorableCachedGrid a) where
    readsPrec = (List.map (first storableCachedGrid) .) . readsPrec

storableCachedGrid :: (Ord a) => Grid a -> StorableCachedGrid a
storableCachedGrid g = StorableCachedGrid g $ pair integral integral $ gridAt g

seededGrid :: Integer -> SeededGrid
seededGrid n = SeededGrid n

seededLookup :: SeededGrid -> (Integer,Integer) -> Integer
seededLookup (SeededGrid n) (x,y) = toInteger $ fst $ next $ mkRNG $
        (fst $ next $ mkRNG (fromInteger $ (x*809) `mod` max_int)) +
        (fst $ next $ mkRNG (fromInteger $ (y*233) `mod` max_int)) +
        (fromInteger $ n `mod` max_int)
    where max_int = toInteger (maxBound :: Int)

data Grid a = CompletelyRandomGrid {
                _grid_seed :: SeededGrid,
                _grid_weights :: [(Integer,a)] }
            | InterpolatedGrid {
                _grid_seed :: SeededGrid,
                _grid_interpolation_weights :: Map (a,a) [(Integer,a)],
                grid_next :: Grid a }
            | ArbitraryReplacementGrid {
                _grid_seed :: SeededGrid,
                _grid_sources :: [(Rational,a)],
                _grid_replacement_weights :: [(Integer,a)],
                grid_next :: Grid a }
            | SpecificPlacementGrid {
                _grid_replacements :: Map (Integer,Integer) a,
                grid_next :: Grid a }
            | CachedGrid (StorableCachedGrid a)
    deriving (Read,Show)

gridAt :: (Ord a) => Grid a -> (Integer,Integer) -> a
gridAt (CompletelyRandomGrid seeded weights) at = fst $ weightedPick weights (mkRNG $ seededLookup seeded at)
gridAt (InterpolatedGrid seeded interpolation_map grid) at@(x,y) =
    let here = gridAt grid (x `div` 2,y `div` 2)
        there = gridAt grid (x `div` 2 + 1,y `div` 2 + 1)
        there_x = gridAt grid (x `div` 2 + 1,y `div` 2)
        there_y = gridAt grid (x `div` 2,y `div` 2 + 1)
        interpolate a1 a2 = fst $ weightedPick (interpolation_map ! (a1,a2)) (mkRNG $ seededLookup seeded at)
        in case (even x,even y) of
                                (True,True) -> here
                                (True,False) -> (interpolate here there_y)
                                (False,True) -> (interpolate here there_x)
                                (False,False) -> (interpolate here there)

gridAt (ArbitraryReplacementGrid seeded sources replacements grid) at =
    case fmap fst $ find ((== here) . snd) sources of
         Just frequency | (seededLookup seeded at `mod` denominator frequency < numerator frequency) ->
	     fst $ weightedPick replacements (mkRNG $ seededLookup seeded at)
	 _ -> here
  where here = gridAt grid at

gridAt (SpecificPlacementGrid rep_map grid) at =
    findWithDefault (gridAt grid at) at rep_map

gridAt (CachedGrid (StorableCachedGrid _ f)) at = f at

cachedGridOf :: (Ord a) => Grid a -> Grid a
cachedGridOf already_cached_grid@(CachedGrid {}) = already_cached_grid
cachedGridOf any_other_grid = CachedGrid $ storableCachedGrid any_other_grid

-- |
-- Generates a random grid.  The first Integer, smoothness,
-- indicates the recursion depth for the generator.  The
-- Integer list is the random integer stream used to generate
-- the map.
generateGrid :: (Ord a) => [(Integer,a)] -> Map (a,a) [(Integer,a)] -> Integer -> [Integer] -> Grid a
generateGrid weights _ 0 seeds = let seed = head seeds
				      in CompletelyRandomGrid (seededGrid seed) weights
generateGrid weights interps n seeds = let seed = head seeds
					    in optimizeGrid $ InterpolatedGrid (seededGrid seed) interps $ 
					                      generateGrid weights interps (n-1) (tail seeds)

-- |
-- Arbitrarily (randomly) replaces some elements of a grid with another.
--
arbitraryReplaceGrid :: (Ord a) => [(Rational,a)] -> [(Integer,a)] -> Integer -> Grid a -> Grid a
arbitraryReplaceGrid sources replacements seed grid = optimizeGrid $
    ArbitraryReplacementGrid (seededGrid seed) sources replacements grid

-- |
-- Replace a specific element of a grid.
--
specificReplaceGrid :: (Integer,Integer) -> a -> Grid a -> Grid a
specificReplaceGrid position x (SpecificPlacementGrid m grid) =
    SpecificPlacementGrid (Map.insert position x m) grid
specificReplaceGrid position x grid = specificReplaceGrid position x $ SpecificPlacementGrid (Map.empty) grid

-- |
-- Strip the cache out of lower layers of the grid, but apply a cache to the top layer.
--
optimizeGrid :: (Ord a) => Grid a -> Grid a
optimizeGrid = cachedGridOf . stripCache
    where stripCache (CachedGrid (StorableCachedGrid g _)) = g
          stripCache g@(CompletelyRandomGrid {}) = g
          stripCache grid = grid { grid_next = stripCache $ grid_next grid }

