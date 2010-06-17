
module Random
    (pick,
     pickM,
     weightedPick,
     weightedPickM,
     linearRoll,
     fixedSumRoll,
     fixedSumLinearRoll,
     logRoll,
     opposedLinearPowerRatio,
     rationalRoll)
    where

import Data.List
import Data.Maybe
import System.Random ()
import Control.Monad.Random
import Control.Monad
import Data.Ratio

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
weightedPickM [] = error "Tried to pick from an empty list."
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

-- | Roll on a rational number that is a probability between zero and one, to generate a boolean.
rationalRoll :: (MonadRandom m) => Rational -> m Bool
rationalRoll r =
    do p <- linearRoll (denominator r - 1)
       return $ p < numerator r

-- | 'opposedLinearPowerRatio' is used when a constant (non-random) power relationship needs to be
-- determined between two parties.  (For example, this is used in the Spot/Hide contest when determining
-- line of sight.)
--
-- It accepts negative values for either parameter, and is invertable, i.e., 
-- @opposedLinearPowerRatio a b@ = @1 - opposedLinearPowerRatio b a@
--
-- One use is: @2 * (a%1) * opposedLinearPowerRatio a b@, whichs gives you roughly @a@ if @a@ and @b@ are equal,
-- or less or more than @a@ otherwise.
opposedLinearPowerRatio :: Integer -> Integer -> Rational
opposedLinearPowerRatio a b | a < 1 = opposedLinearPowerRatio 1 (b-a+1)
opposedLinearPowerRatio a b | b < 1 = opposedLinearPowerRatio (a-b+1) 1
opposedLinearPowerRatio a b | a >= b = ((a-b) % a) + (b % a)/2
opposedLinearPowerRatio a b | otherwise = 1 - opposedLinearPowerRatio b a


