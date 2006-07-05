-- |
-- Why do we need our own random number generation facilities?  Simply
-- because we can't guarantee that the system's random number generator
-- will be identical from one version of the system to the next, and we
-- certainly can't guarantee that between different implementations of
-- the system.  Having our own RNG facility means that we can write
-- a seed value to a save file and generate the same psuedo-random
-- sequence in a completely different environment.  Thus, a map may be
-- defined entirely by its psuedo-random seed.

module RNG
    (randomIntegerStream,
     randomIntegerStreamStream,
     randomIntegerLine,
     randomIntegerGrid)
    where

import Data.List
import HopList

-- |
-- Generates the next in a sequence of psuedo-random Integers.
-- These numbers should not be used raw.  (Due to Insufficient 
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
-- Maps integers in the range [-inf .. inf] to [0 .. inf]
--
bidirect :: Integer -> Integer
bidirect n = if n >= 0
	     then (2*n)
	     else (2*(-n)-1)

-- |
-- An infinite (in both directions) sequence of random Integers, based
-- on a seed.
--
randomIntegerLine :: Integer -> (Integer -> Integer)
randomIntegerLine seed = randomIntegerLine_ (fromList $ randomIntegerStream seed)

randomIntegerLine_ :: HopList Integer -> (Integer -> Integer)
randomIntegerLine_ rands n = rands `index` (bidirect n)

-- |
-- An infinite (in all directions) grid of random Integers, based
-- on a seed.
--
randomIntegerGrid :: Integer -> ((Integer,Integer) -> Integer)
randomIntegerGrid seed = randomIntegerGrid_ $ fromList $ map (fromList . randomIntegerStream) $ randomIntegerStream seed

randomIntegerGrid_ :: HopList (HopList Integer) -> ((Integer,Integer) -> Integer)
randomIntegerGrid_ randss (x,y) = (randss `index` (bidirect y)) `index` (bidirect x)
