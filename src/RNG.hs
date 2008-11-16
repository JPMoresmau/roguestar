
-- |
-- Don't depend on any external source of psuedo-random numbers, because
-- we want to be able to save a psuedo-random seed and know that we can
-- generate the same psuedo-random sequence when we reload it, even across
-- different environments.
--
module RNG
    (mkRNG,
     RNG,
     randomIntegerLine,
     randomIntegerGrid,
     Random(..),
     RandomGen(..))
    where

import Data.List
import ListUtils
import System.Random
import Control.Arrow (first)

newtype RNG = RNG { rng_state :: Integer }

instance RandomGen RNG where
    next g = (fromInteger $ x `quot` (2^24),RNG x)
        where x = (rng_state g * 0x5DEECE66D + 0xB) `mod` (2^48)
    split g = (mkRNG $ fromIntegral x,mkRNG $ fromIntegral y)
        where (x,g') = next g
              (y,_) = next g'
    genRange _ = (0,2^24)

instance Random RNG where
    random = first mkRNG . random
    randomR _ = random 

-- |
-- Construct an RNG from a seed.
--
mkRNG :: Integer -> RNG
mkRNG = RNG . fromIntegral . fst . next . RNG

-- |
-- An infinite (in both directions) sequence of random Integers, based
-- on a seed.
--
randomIntegerLine :: Integer -> (Integer -> Integer)
randomIntegerLine = bidirectionalAccessor1D . randoms . mkRNG

-- |
-- An infinite (in all directions) grid of random Integers, based
-- on a seed.
--
randomIntegerGrid :: Integer -> ((Integer,Integer) -> Integer)
randomIntegerGrid = bidirectionalAccessor2D . map (randoms :: RNG -> [Integer]) . randoms . mkRNG
