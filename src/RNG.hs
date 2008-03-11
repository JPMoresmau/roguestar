
-- |
-- Don't depend on any external source of psuedo-random numbers, because
-- we want to be able to save a psuedo-random seed and know that we can
-- generate the same psuedo-random sequence when we reload it.
--
module RNG
    (randomIntegerStream,
     randomIntegerStreamStream,
     randomIntegerLine,
     randomIntegerGrid)
    where

import Data.List
import ListUtils

-- |
-- Generates the next in a sequence of psuedo-random Integers.
-- These numbers should not be used raw.  (Due to insufficient
-- "random-ness" of the least significant bit.)  Use a
-- randomIntegerStream[Stream].
--
nextRandomSeed :: Integer -> Integer
nextRandomSeed x = (x * 0x5DEECE66D + 0xB) `mod` (2^48)

-- |
-- A stream of random integers from a seed.
--
randomIntegerStream :: Integer -> [Integer]
randomIntegerStream x = let nri = nextRandomSeed x
                            in (nri `quot` 24) : (randomIntegerStream nri)

-- |
-- A stream of random integer streams.  Good when you need to do
-- a lot of splitting.
--
randomIntegerStreamStream :: Integer -> [[Integer]]
randomIntegerStreamStream x = let nri1 = nextRandomSeed x
                                  nri2 = nextRandomSeed nri1
                                  in (randomIntegerStream (nri1 + 1) :
                                      (randomIntegerStreamStream (nri2 - 1)))

-- |
-- An infinite (in both directions) sequence of random Integers, based
-- on a seed.
--
randomIntegerLine :: Integer -> (Integer -> Integer)
randomIntegerLine seed = bidirectionalAccessor1D $ randomIntegerStream seed

-- |
-- An infinite (in all directions) grid of random Integers, based
-- on a seed.
--
randomIntegerGrid :: Integer -> ((Integer,Integer) -> Integer)
randomIntegerGrid seed = bidirectionalAccessor2D $ map randomIntegerStream $ randomIntegerStream seed
