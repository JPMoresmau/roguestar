module RandomUtils
    (pick,
     weightedPick)
    where

import Data.List
import Data.Maybe

pick :: Integer -> [a] -> a
pick seed elems = elems `genericIndex` (seed `mod` (genericLength elems))

weightedPick :: Integer -> [(Integer,a)] -> a
weightedPick seed elems = let (weights,values) = unzip elems
                              (weightTotal,weightTotals) = mapAccumL (\x y -> (x+y,x+y)) 0 weights
                              weightToFind = seed `mod` weightTotal
                              index = fromJust $ findIndex (\x -> x > weightToFind) weightTotals
                              in values !! index