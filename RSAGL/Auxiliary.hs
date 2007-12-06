module RSAGL.Auxiliary
    (doubles,
     loopedDoubles,
     consecutives,
     loopedConsecutives,
     dropRandomElements,
     matrixMultiplyImpl,
     loopList,
     shiftR,
     zeroToOne,
     constrain)
    where

import Data.Array
import System.Random

-- pairify converts a list of length two into a pair.

pairify :: [a] -> (a,a)
pairify (m:n:[]) = (m,n)
pairify _ = error "pairify only works on lists of length two"

-- doubles transforms a list to a list of adjacent elements.

-- doubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5)]

doubles :: [a] -> [(a,a)]
doubles = (map pairify) . (consecutives 2)

-- loopedDoubles transforms a list to a list of adjacent elements, looping back to the beginning of the list.

-- loopedDoubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5),(5,1)]

loopedDoubles :: [a] -> [(a,a)]
loopedDoubles = (map pairify) . (loopedConsecutives 2)

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

-- matrixMultiplyImpl implements matrix multiplication.

matrixMultiplyImpl :: (b -> c -> c) -> c -> (a -> a -> b) -> [[a]] -> [[a]] -> [[c]]
matrixMultiplyImpl addOp zero multiplyOp m n = [[foldr addOp zero $ zipWith multiplyOp m' n' | n' <- n] | m' <- m]

-- loopList appends the first element of a list to the end of the list.  The result is a list of one greater length.

-- shiftR shifts every element of a list to the right, recyling the last element as the first.  The result is a list of the same length.

loopList :: [a] -> [a]
loopList ps = ps ++ [head ps]

shiftR :: [a] -> [a]
shiftR ps = last ps : init ps

-- zeroToOne creates a list of numbers from 0.0 to 1.0, using n steps.

zeroToOne :: Integer -> [Double]
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
