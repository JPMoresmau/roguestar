module Dice (testDice, d)
    where

import Control.Monad.State
import DB
import Tests

--
-- Answers a result of rolling n s-sided dice, numbered 1 to s.
--
d :: Integer -> Integer -> DB Integer
d 0 _ = do return 0
d 1 s | (s > 0)           = do roll <- dbNextRandomInteger
	                       return ((roll `mod` s) + 1)
d n s | (n >= 2 && s > 0) = do roll <- d 1 s
		               rolls <- d (n-1) s
		               return (roll + rolls)
d _ _ = error "Rolled negative dice or negative sided dice."

--
-- Test one die roll.  This does not test that die rolls are "sufficiently" random,
-- only the results fall within possible ranges.
--
testDieRoll :: Integer -> Integer -> TestCase
testDieRoll n s = do db0 <- initial_db
		     let dieRoll = evalState (n `d` s) db0
			 testStr = ("testDice " ++ show n ++ "d" ++ show s ++ "(" ++ show dieRoll ++ ")")
			 in return (if dieRoll >= n && dieRoll <= n*s
				    then Passed testStr
				    else Failed testStr)

--
-- Several dice related test cases.
--
testDice_ :: [TestCase]
testDice_ = 
    [ testDieRoll 1 10,
      testDieRoll 2 5,
      testDieRoll 0 3,
      testDieRoll 15 23 ]

testDice :: [TestCase]
testDice = foldr1 (++) (replicate 4 testDice_)