module RandomUtils
    ()
    where

pick :: Integer -> [a] -> a
pick seed elems = elems `genericIndex` (seed `mod` (length elems))

weightedPick :: Integer -> [(Integer,a)] -> a
weightedPick seed elems = let (weights,values) = unzip elems
                              (weightTotal,weightTotals) = mapAccumL (\x y -> (x+y,x+y)) 0 weights
                              weightToFind = seed `mod` weightTotal
                              index = just $ findIndex (\x -> x > weight) weightTotals
                              in values !! index