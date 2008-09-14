module RSAGL.Auxiliary
    (doubles,
     loopedDoubles,
     consecutives,
     loopedConsecutives,
     dropRandomElements,
     loopList,
     shiftR,
     zeroToOne,
     constrain,
     debugTime,
     waitParList)
    where

import Data.Array
import System.Random
import System.CPUTime
import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace

-- doubles transforms a list to a list of adjacent elements.

-- doubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5)]

doubles :: [a] -> [(a,a)]
doubles [] = []
doubles [_] = []
doubles (x:y:zs) = (x,y) : doubles (y:zs)

-- loopedDoubles transforms a list to a list of adjacent elements, looping back to the beginning of the list.

-- loopedDoubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5),(5,1)]

loopedDoubles :: [a] -> [(a,a)]
loopedDoubles as = loopedDoubles_ (head as) as
    where loopedDoubles_ _ [] = []
          loopedDoubles_ a [x] = [(x,a)]
          loopedDoubles_ a (x:y:zs) = (x,y) : loopedDoubles_ a (y:zs)

-- consecutives answers a list containing every sequence of n consecutive
-- elements in the parameter.

--consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4]]

consecutives :: Int -> [a] -> [[a]]
consecutives n xs = let taken = take n xs
			in if (length taken == n)
			   then (taken : (consecutives n $ tail xs))
			   else []

-- loopedConsecutives answers a list containing every sequence of n consecutive
-- elements in the parameter, looping back to the beginning of the list.

-- consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4],[3,4,1],[4,1,2]]

loopedConsecutives :: Int -> [a] -> [[a]]
loopedConsecutives n xs = consecutives n $ take (n + length xs - 1) $ cycle xs

-- dropRandomElements removes some elements of a list at random.  The first parameter is the number of elements out of 100 that should be included (not dropped).
-- The second is the random number generator, and the third is the list to be operated on.

dropRandomElements :: Int -> StdGen -> [a] -> [a]
dropRandomElements percent _ _ | percent > 100 = error "dropRandomElements: percent > 100"
dropRandomElements percent _ _ | percent < 0 = error "dropRandomElements: percent < 100"
dropRandomElements _ _ [] = []
dropRandomElements percent rand_ints things = 
    let (next_int,next_gen) = next rand_ints
	rest = dropRandomElements percent next_gen (tail things)
	in if (next_int `mod` 100 < percent)
	   then ((head things) : rest)
	   else rest

-- loopList appends the first element of a list to the end of the list.  The result is a list of one greater length.

-- shiftR shifts every element of a list to the right, recyling the last element as the first.  The result is a list of the same length.

loopList :: [a] -> [a]
loopList ps = ps ++ [head ps]

shiftR :: [a] -> [a]
shiftR ps = last ps : init ps

-- zeroToOne creates a list of numbers from 0.0 to 1.0, using n steps.

zeroToOne :: Integer -> [Double]
zeroToOne n | n > 100000 = trace ("Warning: zeroToOne was asked for " ++ show n ++ " subdivisions, which seems high.  Using 100,000 instead.") zeroToOne 100000
zeroToOne n | n <= 1000 = ztos ! n
zeroToOne n = zeroToOnePrim n

zeroToOnePrim :: Integer -> [Double]
zeroToOnePrim n = map (*x) [0..(fromInteger $ n-1)]
    where x = recip (fromInteger $ n - 1)

ztos :: Array Integer [Double]
ztos = listArray (0,1000) $ map (zeroToOnePrim) [0 .. 1000]

-- constrain restricts the effective domain of a function.  Outside of the restricted domain, the function is id

constrain :: (a -> Bool) -> (a -> a) -> a -> a
constrain f g x = if f x then g x else x

-- debugTime prints a statement indicating how long an IO action takes to complete

debugTime :: String -> IO a -> IO a
debugTime msg io_action =
    do print $ "debugTime: starting " ++ msg
       start_time <- getCPUTime
       result <- io_action
       end_time <- getCPUTime
       print $ "debugTime: done with " ++ msg ++ " " ++ show (end_time - start_time)
       return result

-- as parList, but waits until the list is fully evaluated.

waitParList :: Strategy a -> Strategy [a]
waitParList s a = parList s a `pseq` seqList s a
