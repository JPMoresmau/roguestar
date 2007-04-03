{-# OPTIONS_GHC -farrows #-}

--
-- These are some crude sanity tests of arrow system.
-- Some of these are particularly poor examples of how to use the arrows being tested.
--

module RSAGL.Tests
    (main)
    where

import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.ThreadedArrow as ThreadedArrow
import Control.Arrow.Operations
import Control.Arrow
import Data.Set as Set
import Data.List as List
import Data.Monoid

--
-- State machine that adds its input to its state
--
countingArrow :: Integer -> StatefulFunction Integer Integer
countingArrow = stateContext $ 
                    proc x -> do y <- fetch -< ()
                                 store -< x + y
                                 fetch -< ()

--
-- State machine that is true iff the number of False inputs it has recieved is even
--
evenZeroesArrow :: SwitchedFunction Bool Bool Bool Bool
evenZeroesArrow = proc x ->
     do case x of
               False -> SwitchedArrow.switchTerminate -< (evenZeroesArrow_oddZeroes,False)
               True -> returnA -< True

evenZeroesArrow_oddZeroes :: SwitchedFunction Bool Bool Bool Bool
evenZeroesArrow_oddZeroes = proc x ->
    do case x of
              False -> SwitchedArrow.switchTerminate -< (evenZeroesArrow,True)
              True -> returnA -< False

--
-- A cellular automata that spawns the two adjacent cells (represented by Integers) on each iteration.
-- Cells at non-zero integers divisible by 3 are "sticky;" once reached they persist forever.
-- All other integers die out after spawning.
--
spawnPlusAndMinusAndDie :: Integer -> ThreadedFunction () (Set Integer) () (Set Integer)
spawnPlusAndMinusAndDie i = step1
    where step1 = proc () ->
              do ThreadedArrow.switchTerminate -< (step2,mempty)
          step2 = proc () ->
              do spawnThreads -< [spawnPlusAndMinusAndDie (i+1),spawnPlusAndMinusAndDie (i-1)]
                 killThreadIf -< (i `mod` 3 /= 0 || i == 0,Set.singleton i)

--
-- Sanity test of the StatefulArrow.
--
addFive :: Integer -> Integer
addFive x = let (_,sf1) = runStatefulArrow (countingArrow x) 1
                (_,sf2) = runStatefulArrow sf1 3
                (_,sf3) = runStatefulArrow sf2 (-1)
                (_,sf4) = runStatefulArrow sf3 1
                in fst $ runStatefulArrow sf4 1

--
--Sanity test of the SwitchedArrow.
--
evenZeroes :: [Bool] -> Bool
evenZeroes = last . runStateMachine (SwitchedArrow.statefulForm evenZeroesArrow)

--
-- Sanity test of the ThreadedArrow
--
spawnPMD :: Int -> Set Integer
spawnPMD n = (!! n) $ runStateMachine (ThreadedArrow.statefulForm $ [spawnPlusAndMinusAndDie 0]) $ replicate (n+1) ()

test :: (Eq a,Show a) => a -> a -> IO ()
test actual expected | actual == expected = 
                       do putStrLn "Test Case Passed"
                          putStrLn ""
test actual expected = do putStrLn "TEST CASE FAILED"
                          putStrLn $ "expected: " ++ show expected
                          putStrLn $ "actual:   " ++ show actual
                          putStrLn ""

main :: IO ()
main = do putStrLn "add five test (sanity test of StatefulArrow)"
          (addFive 2) `test` 7
          putStrLn "even zeroes test (sanity test of SwitchedArrow)"
          (evenZeroes [True,True,False,True,False]) `test` True
          putStrLn "odd zeroes test (sanity test of SwitchedArrow)"
          evenZeroes [True,True,True,False,False,True,False,True] `test` False
          putStrLn "spawning test 1 (sanity test of ThreadedArrow)"
          spawnPMD 1 `test` Set.fromList [0]
          putStrLn "spawning test 2 (sanity test of ThreadedArrow)"
          spawnPMD 2 `test` Set.fromList [-1,1]
          putStrLn "spawning test 3 (sanity test of ThreadedArrow)"
          spawnPMD 3 `test` Set.fromList [0,-2,2]
          putStrLn "spawning test 4 (sanity test of ThreadedArrow)"
          spawnPMD 4 `test` Set.fromList [-3,-1,1,3]
          putStrLn "spawning test 5 (does killThreadIf block correctly?)"
          spawnPMD 5 `test` Set.fromList [-4,-3,-2,0,2,3,4]

