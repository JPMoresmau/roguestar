{-# OPTIONS_GHC -farrows #-}

--
-- Some basic tests of the FRP arrow system.
--

module RSAGL.Tests
    (main)
    where

import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.ThreadedArrow as ThreadedArrow
import RSAGL.FRP as FRP
import RSAGL.Edge as Edge
import RSAGL.Time
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
              do ThreadedArrow.spawnThreads -< [spawnPlusAndMinusAndDie (i+1),spawnPlusAndMinusAndDie (i-1)]
                 ThreadedArrow.killThreadIf -< (i `mod` 3 /= 0 || i == 0,Set.singleton i)

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

testIntegral :: IO ()
testIntegral = test "testIntegral"
                  (frpTest [arr (\x -> [x]) <<< threadTime] (replicate 16 ()))
                  [[0.0],[0.1],[0.2],[0.3],[0.4],[0.5],[0.6],[0.7],[0.8],[0.9],[1.0],[1.1],[1.2],[1.3],[1.4],[1.5]] 

testDerivative :: IO ()
testDerivative = test "testDerivative"
                    (frpTest [arr (\x -> [x]) <<< derivative]
                             [5.0,6.0,8.0,11.0,15.0,20.0,26.0,33.0])
                    [[0],[10],[20],[30],[40],[50],[60],[70]]

testInitial :: IO ()
testInitial = test "testInitial"
                 (frpTest [arr (\x -> [x]) <<< initial]
                          [5,7,2,1,6,3,4])
                 [[5],[5],[5],[5],[5],[5],[5]]

testEdgeFold :: IO ()
testEdgeFold = test "testEdgeFold"
                  (frpTest [arr (\x -> [x]) <<< edgeFold 0 (+)]
                           [3,2,2,2,2,4,4,3,8,7,6,6])
                  [[3],[5],[5],[5],[5],[9],[9],[12],[20],[27],[33],[33]]

testEdgeMap :: IO ()
testEdgeMap = test "testEdgeMap"
                 (frpTest [arr (\x -> [x]) <<< edgeMap (*2)]
                          [2,2,2,4,1,3,9,5,5,5,3])
                 [[4],[4],[4],[8],[2],[6],[18],[10],[10],[10],[6]]

testHistory :: IO ()
testHistory = test "testHistory"
                 (frpTest [history (fromSeconds 0.3)]
                          [2,3,1,1,2,2,2,7,7,7,7,7])
                 [[edge0],
                  [edge1,edge0],
                  [edge2,edge1,edge0],
                  [edge2,edge1,edge0],
                  [edge3,edge2,edge1],
                  [edge3,edge2,edge1],
                  [edge3,edge2,edge1],
                  [edge4,edge3],
                  [edge4,edge3],
                  [edge4,edge3],
                  [edge4,edge3],
                  [edge4,edge3]]
    where edge0 = Edge { edge_previous = 2,
                         edge_next = 2,
                         edge_changed = 0.0 }
          edge1 = Edge { edge_previous = 2,
                         edge_next = 3,
                         edge_changed = 0.1 }
          edge2 = Edge { edge_previous = 3,
                         edge_next = 1,
                         edge_changed = 0.2 }
          edge3 = Edge { edge_previous = 1,
                         edge_next = 2,
                         edge_changed = 0.4 }
          edge4 = Edge { edge_previous = 2,
                         edge_next = 7,
                         edge_changed = 0.7 }

testEdgep :: IO ()
testEdgep = test "testEdgep"
                 (frpTest [arr (\x -> [x]) <<< edgep]
                          [2,2,2,3,2,3,3,3,4,4,4,4,5,4,5,5])
                 [[False],[False],[False],[True],[True],[True],[False],[False],[True],[False],[False],[False],
                  [True],[True],[True],[False]]

test :: (Eq a,Show a) => String -> a -> a -> IO ()
test name actual expected | actual == expected = 
                       do putStrLn $ "Test Case Passed: " ++ name
test name actual expected = 
                       do putStrLn ""
                          putStrLn $ "TEST CASE FAILED: " ++ name
                          putStrLn $ "expected: " ++ show expected
                          putStrLn $ "actual:   " ++ show actual
                          putStrLn ""

main :: IO ()
main = do test "add five test (sanity test of StatefulArrow)" 
               (addFive 2) 7
          test "even zeroes test (sanity test of SwitchedArrow)" 
               (evenZeroes [True,True,False,True,False]) True
          test "odd zeroes test (sanity test of SwitchedArrow)" 
               (evenZeroes [True,True,True,False,False,True,False,True]) False
          test "spawning test 1 (sanity test of ThreadedArrow)"
               (spawnPMD 1) (Set.fromList [0])
          test "spawning test 2 (sanity test of ThreadedArrow)"
               (spawnPMD 2) (Set.fromList [-1,1])
          test "spawning test 3 (sanity test of ThreadedArrow)"
               (spawnPMD 3) (Set.fromList [0,-2,2])
          test "spawning test 4 (sanity test of ThreadedArrow)"
               (spawnPMD 4) (Set.fromList [-3,-1,1,3])
          test "spawning test 5 (does killThreadIf work conditionally?)"
               (spawnPMD 5) (Set.fromList [-4,-3,-2,0,2,3,4])
          testIntegral
          testDerivative
          testInitial
          testEdgeFold
          testEdgeMap
          testHistory
          testEdgep