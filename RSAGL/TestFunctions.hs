{-# OPTIONS_GHC -farrows #-}

--
-- These are some crude sanity tests of arrow system.
-- Some of these are particularly poor examples of how to use the arrows being tested.
--

module RSAGL.TestFunctions
    (addFive)
    where

import RSAGL.StatefulArrow
import RSAGL.SwitchedArrow
import Control.Arrow.Operations
import Control.Arrow

countingArrow :: Integer -> StatefulFunction Integer Integer
countingArrow = stateContext $ 
                    proc x -> do y <- fetch -< ()
                                 store -< x + y
                                 fetch -< ()

evenZeroesArrow :: SwitchedFunction Bool Bool Bool Bool
evenZeroesArrow = proc x ->
     do case x of
               False -> switchTerminate evenZeroesArrow_oddZeroes -< False
               True -> returnA -< True

evenZeroesArrow_oddZeroes :: SwitchedFunction Bool Bool Bool Bool
evenZeroesArrow_oddZeroes = proc x ->
    do case x of
              False -> switchTerminate evenZeroesArrow -< True
              True -> returnA -< False

-- |
-- Sanity test of the StatefulArrow.
--
addFive :: Integer -> Integer
addFive x = let (_,sf1) = runStatefulArrow (countingArrow x) 1
                (_,sf2) = runStatefulArrow sf1 1
                (_,sf3) = runStatefulArrow sf2 1
                (_,sf4) = runStatefulArrow sf3 1
                in fst $ runStatefulArrow sf4 1

-- |
--Sanity test of the SwitchedArrow.
--
evenZeroes :: [Bool] -> Bool
evenZeroes = last . runStateMachine (switchedContext evenZeroesArrow)

