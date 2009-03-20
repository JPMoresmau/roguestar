
module Random
    (pick,
     pickM,
     weightedPick,
     weightedPickM,
     linearRoll,
     fixedSumRoll,
     fixedSumLinearRoll,
     logRoll)
    where

import Data.List
import Data.Maybe
import System.Random ()
import Control.Monad.Random
import Control.Monad

-- | Pick an element of a list at random.
pick :: (RandomGen g) => [a] -> g -> (a,g)
pick elems = runRand (pickM elems)

-- | Pick an element of a weighted list at random.  E.g. in "[(2,x),(3,y)]" "y" will be picked three times out of five while "x" will be picked 2 times out of five.
weightedPick :: (RandomGen g) => [(Integer,a)] -> g -> (a,g)
weightedPick elems = runRand (weightedPickM elems)

-- | 'pick' in MinadRandom
pickM :: (MonadRandom m) => [a] -> m a
pickM elems = weightedPickM (map (\x -> (1,x)) elems)

-- | 'weightedPick' in MonadRandom
weightedPickM :: (MonadRandom m) => [(Integer,a)] -> m a
weightedPickM elems = 
    do let (weights,values) = unzip elems
       let (weight_total,weight_totals) = mapAccumL (\x y -> (x+y,x+y)) 0 weights
       weight_to_find <- getRandomR (1,weight_total)
       let index = fromJust $ findIndex (\x -> x >= weight_to_find) weight_totals
       return $ values !! index

-- | Roll an (n+1) sided die numbered zero to n.
linearRoll :: (MonadRandom m) => Integer -> m Integer
linearRoll n = getRandomR (0,n)

-- | fixedSumRoll using 'linearRoll', with optimizations.
-- REVISIT: this can be improved significantly, but performance doesn't seem to be a material problem so far.
fixedSumLinearRoll :: (MonadRandom m) => [Integer] -> Integer -> m [Integer]
fixedSumLinearRoll xs a = fixedSumRoll (map (linearRoll . min a) xs) a

-- | Roll a sequence of random variables, such that the sum of the result is a fixed value.
fixedSumRoll :: (MonadRandom m) => [m Integer] -> Integer -> m [Integer]
fixedSumRoll rs a = 
    do xs <- sequence rs
       case sum xs == a of
           True -> return xs
           False -> fixedSumRoll rs a

-- | Roll a die where the typical outcome is the base-2 logarithm of the input.
-- This function has exactly the same probability of rolling exactly 0 as 'linearDiceRoll'.
--
logRoll :: (MonadRandom m) => Integer -> m Integer
logRoll n = liftM (min n) $ accumRoll 0 n
    where accumRoll c x = 
              do x' <- linearRoll x
                 case x' of
                     0 -> return c
                     _ -> accumRoll (c+1) x'
