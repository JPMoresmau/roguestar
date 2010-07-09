module RSAGL.Math.ListUtils
    (doubles,
     loopedDoubles,
     consecutives,
     loopedConsecutives,
     zeroToOne)
    where

import RSAGL.Math.Types
import Debug.Trace

-- | Transforms a list to a list of adjacent elements.
--
-- @doubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5)]@
doubles :: [a] -> [(a,a)]
doubles [] = []
doubles [_] = []
doubles (x:y:zs) = (x,y) : doubles (y:zs)

-- | loopedDoubles transforms a list to a list of adjacent elements, looping
-- back to the beginning of the list.
--
-- @loopedRSdoubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5),(5,1)]@
loopedDoubles :: [a] -> [(a,a)]
loopedDoubles as = loopedDoubles_ (head as) as
    where loopedDoubles_ _ [] = []
          loopedDoubles_ a [x] = [(x,a)]
          loopedDoubles_ a (x:y:zs) = (x,y) : loopedDoubles_ a (y:zs)

-- | Answers a list containing every sequence of n consecutive
-- elements in the parameter.
--
-- @consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4]]@
consecutives :: Int -> [a] -> [[a]]
consecutives n xs = let taken = take n xs
                        in if (length taken == n)
                           then (taken : (consecutives n $ tail xs))
                           else []

-- | Answers a list containing every sequence of n consecutive
-- elements in the parameter, looping back to the beginning of the list.
--
-- @consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4],[3,4,1],[4,1,2]]@
loopedConsecutives :: Int -> [a] -> [[a]]
loopedConsecutives n xs = consecutives n $ take (n + length xs - 1) $ cycle xs

-- | Creates a list of numbers from 0.0 to 1.0, using n steps.
-- This can't be done with the enum-from-to method, due to roundoff errors.
zeroToOne :: Integer -> [RSdouble]
zeroToOne n | n > 100000 = trace ("Warning: zeroToOne was asked for " ++ show n ++ " subdivisions, which seems high.  Using 100,000 instead.") zeroToOne 100000
zeroToOne n = map (*x) [0..(fromInteger $ n-1)]
    where x = recip (fromInteger $ n - 1)

