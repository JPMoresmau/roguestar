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
import RSAGL.Angle
import RSAGL.ListUtils
import RSAGL.Vector
import RSAGL.Matrix
import Control.Arrow.Operations
import Control.Arrow
import Data.Set as Set
import Data.List as List
import Data.Monoid
import Test.QuickCheck hiding (test)

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

testRadiansToDegrees :: IO ()
testRadiansToDegrees = testClose "testRadiansToDegrees"
                          (toDegrees $ fromRadians (pi/6))
                          30
                          0.001

testDegreesToRadians :: IO ()
testDegreesToRadians = testClose "testDegreesToRadians"
                          (toRadians $ fromDegrees 270)
                          (-pi/2)
                          0.001

testAngleAdd :: IO ()
testAngleAdd = testClose "testAngleAdd"
                   (toDegrees $ fromDegrees 100 + fromDegrees 90)
                   (-170)
                   0.001

testAngleSubtract :: IO ()
testAngleSubtract = testClose "testAngleSubtract"
                        (toDegrees $ fromDegrees (-20) - fromDegrees 400)
                        (-60)
                        0.001

testDoubles :: IO ()
testDoubles = test "testDoubles"
                             (doubles [1,2,3,4])
                             [(1,2),(2,3),(3,4)]

testLoopedDoubles :: IO ()
testLoopedDoubles = test "testLoopedDoubles"
                             (loopedDoubles [1,2,3,4])
                             [(1,2),(2,3),(3,4),(4,1)]

testConsecutives :: IO ()
testConsecutives = test "testConsecutives"
                             (consecutives 3 [1,2,3,4])
                             [[1,2,3],[2,3,4]]

testShortConsecutives :: IO ()
testShortConsecutives = test "testShortConsecutives"
                            (consecutives 3 [1,2])
                            []

testLoopedConsecutives :: IO ()
testLoopedConsecutives = test "testLoopedConsecutives"
                             (loopedConsecutives 3 [1,2,3,4])
                             [[1,2,3],[2,3,4],[3,4,1],[4,1,2]]

testShortLoopedConsecutives :: IO ()
testShortLoopedConsecutives = test "testShortLoopedConsecutives"
                             (loopedConsecutives 3 [1,2])
                             [[1,2,1],[2,1,2]]

testAngleBetween :: IO ()
testAngleBetween = testClose "testAngleBetween"
                   (angleBetween (vector3d (-1,1,0)) (vector3d (0,0,1)))
                   (fromDegrees (90))
                   (fromDegrees (0.001))

testDistanceBetween :: IO ()
testDistanceBetween = testClose "testDistanceBetween"
                      (distanceBetween (vector3d (-1,1,0)) (vector3d (0,0,1)))
                      (sqrt 3)
                      0.001

-- we just test that the right-hand rule is observed
testCrossProduct :: IO ()
testCrossProduct =
    do let (x,y,z) = toXYZ $ crossProduct (vector3d (1,0,0)) (vector3d (0,1,0))
       testClose "testCrossProduct(x)" x 0.0 0.001
       testClose "testCrossProduct(y)" y 0.0 0.001
       testClose "testCrossProduct(z)" z 1.0 0.001

-- here we use quickCheck to confirm that the crossProduct always yields a vector orthagonal
-- to the parameters.
quickCheckCrossProductByAngleBetween :: IO ()
quickCheckCrossProductByAngleBetween = 
    do putStr "quickCheckCrossProductByAngleBetween: "
       quickCheck _qccpbab
           where _qccpbab (v1,v2) = let cp = crossProduct (vector3d v1) (vector3d v2)
                                        a1 = toRadians $ angleBetween cp (vector3d v1) :: Double
                                        a2 = toRadians $ angleBetween cp (vector3d v2) :: Double
                                        in abs (a1 - (pi/2)) < 0.001 &&
                                           abs (a2 - (pi/2)) < 0.001

testVectorAverage :: IO ()
testVectorAverage =
    do let (x,y,z) = toXYZ $ vectorAverage [vector3d (0.1,0,0),vector3d (0,-2,0),vector3d (0,0,5)]
       testClose "testVectorAverage(x)" x 0.57735 0.001
       testClose "testVectorAverage(y)" y (-0.57735) 0.001
       testClose "testVectorAverage(z)" z 0.57735 0.001

testNewell :: IO ()
testNewell =
    do let (x,y,z) = toXYZ $ newell [point3d (1,0,0),point3d (0,1,0),point3d (0,0,-1)]
       testClose "testNewell(x)" x (-0.57735) 0.001
       testClose "testNewell(y)" y (-0.57735) 0.001
       testClose "testNewell(z)" z 0.57735 0.001

type Double2 = (Double,Double)
type Double22 = (Double2,Double2)
type Double3 = (Double,Double,Double)
type Double33 = (Double3,Double3,Double3)
type Double4 = (Double,Double,Double,Double)
type Double44 = (Double4,Double4,Double4,Double4)

d2ToMatrix :: Double22 -> Matrix
d2ToMatrix ((a,b),(c,d)) = matrix $ [[a,b],[c,d]]

d3ToMatrix :: Double33 -> Matrix
d3ToMatrix ((a,b,c),(d,e,f),(g,h,i)) = matrix $ [[a,b,c],[d,e,f],[g,h,i]]

d4ToMatrix :: Double44 -> Matrix
d4ToMatrix ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = matrix $ [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]

testDeterminant2 :: IO ()
testDeterminant2 =
   do test "testDeterminant2-1"
           (determinant $ matrix [[1,5],[3,0]])
           (-15)
      test "testDeterminant2-2"
           (determinant $ matrix [[-2,0],[0.5,1]])
           (-2)
      test "testDeterminant2-3"
           (determinant $ matrix [[0,0.5],[0.5,0.5]])
           (-0.25)

testDeterminant3 :: IO ()
testDeterminant3 =
   do test "testDeterminant3-1"
           (determinant $ matrix [[5,-1,0.5],[-3,4,2],[0,0,-1]])
           (-17)
      testClose "testDeterminant3-2" 0.001
           (determinant $ matrix [[0.5,-0.5,-2.0/3.0],
                                  [-2.5,-1.0,-3.0],
                                  [-0.5,0.5,-4.0/3.0]])
           3.5

testDeterminant4 :: IO ()
testDeterminant4 =
   do test "testDeterminant4-1"
           (determinant $ matrix [[1.5,-1.0,0.5,-2.0],[1.0,1.5,1.0,1.0],[1.5,1.0,0.5,-0.5],[1.0,1.5,1.0,0.0]])
           2

quickCheckMatrixDeterminant2 :: IO ()
quickCheckMatrixDeterminant2 =
    do putStr "quickCheckMatrixDeterminant2: "
       quickCheck _qcmi
           where _qcmi :: Double22 -> Bool
                 _qcmi m = 
                     determinant (matrixTranspose mat) `equalClose` determinant mat
                         where mat = d2ToMatrix m

quickCheckMatrixDeterminant3 :: IO ()
quickCheckMatrixDeterminant3 =
    do putStr "quickCheckMatrixDeterminant3: "
       quickCheck _qcmi
           where _qcmi :: Double33 -> Bool
                 _qcmi m = 
                     determinant (matrixTranspose mat) `equalClose` determinant mat
                         where mat = d3ToMatrix m

quickCheckMatrixDeterminant4 :: IO ()
quickCheckMatrixDeterminant4 =
    do putStr "quickCheckMatrixDeterminant4: "
       quickCheck _qcmi
           where _qcmi :: Double44 -> Bool
                 _qcmi m = 
                     determinant (matrixTranspose mat) `equalClose` determinant mat
                         where mat = d4ToMatrix m

quickCheckMatrixMultiplyDeterminant2 :: IO ()
quickCheckMatrixMultiplyDeterminant2 =
    do putStr "quickCheckMatrixMultiplyDeterminant2: "
       quickCheck _qcmmd 
            where _qcmmd :: (Double22,Double22) -> Bool
                  _qcmmd (m1,m2) =
                       (determinant (mat1 `matrixMultiply` mat2)) `equalClose` (determinant mat1 * determinant mat2)
                           where mat1 = d2ToMatrix m1
                                 mat2 = d2ToMatrix m2

quickCheckMatrixMultiplyDeterminant3 :: IO ()
quickCheckMatrixMultiplyDeterminant3 =
    do putStr "quickCheckMatrixMultiplyDeterminant3: "
       quickCheck _qcmmd 
            where _qcmmd :: (Double33,Double33) -> Bool
                  _qcmmd (m1,m2) =
                       (determinant (mat1 `matrixMultiply` mat2)) `equalClose` (determinant mat1 * determinant mat2)
                           where mat1 = d3ToMatrix m1
                                 mat2 = d3ToMatrix m2

quickCheckMatrixMultiplyDeterminant4 :: IO ()
quickCheckMatrixMultiplyDeterminant4 =
    do putStr "quickCheckMatrixMultiplyDeterminant4: "
       quickCheck _qcmmd 
            where _qcmmd :: (Double44,Double44) -> Bool
                  _qcmmd (m1,m2) =
                       (determinant (mat1 `matrixMultiply` mat2)) `equalClose` (determinant mat1 * determinant mat2)
                           where mat1 = d4ToMatrix m1
                                 mat2 = d4ToMatrix m2

quickCheckMatrixInverse2 :: IO ()
quickCheckMatrixInverse2 =
    do putStr "quickCheckMatrixInverse2: "
       quickCheck _qcmi
           where _qcmi :: Double22 -> Bool
                 _qcmi ((a,b),(e,f)) = 
                     if determinant mat `equalClose` 0
                     then True
                     else matrixInversePrim (matrixInversePrim mat) `matrixEqualClose` mat
                         where mat = matrix [[a,b],[e,f]]

quickCheckMatrixInverse3 :: IO ()
quickCheckMatrixInverse3 =
    do putStr "quickCheckMatrixInverse3: "
       quickCheck _qcmi
           where _qcmi :: Double33 -> Bool
                 _qcmi ((a,b,c),(e,f,g),(i,j,k)) = 
                     if determinant mat `equalClose` 0
                     then True
                     else matrixInversePrim (matrixInversePrim mat) `matrixEqualClose` mat
                         where mat = matrix [[a,b,c],[e,f,g],[i,j,k]]

quickCheckMatrixInverse4 :: IO ()
quickCheckMatrixInverse4 =
    do putStr "quickCheckMatrixInverse4: "
       quickCheck _qcmi
           where _qcmi :: Double44 -> Bool
                 _qcmi ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = 
                     if determinant mat `equalClose` 0
                     then True
                     else matrixInversePrim (matrixInversePrim mat) `matrixEqualClose` mat
                         where mat = matrix [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]

test :: (Eq a,Show a) => String -> a -> a -> IO ()
test name actual expected | actual == expected = 
                       do putStrLn $ "Test Case Passed: " ++ name
test name actual expected = 
                       do putStrLn ""
                          putStrLn $ "TEST CASE FAILED: " ++ name
                          putStrLn $ "expected: " ++ show expected
                          putStrLn $ "actual:   " ++ show actual
                          putStrLn ""

equalClose :: (Eq a,Num a,Ord a,Fractional a) => a -> a -> Bool
equalClose actual expected = abs (actual - expected) < 0.01

matrixEqualClose :: Matrix -> Matrix -> Bool
matrixEqualClose m n = and $ List.map and $ zipWith (zipWith equalClose)
    (rowMajorForm m) (rowMajorForm n)

testClose :: (Eq a,Show a,Num a,Ord a) => String -> a -> a -> a -> IO ()
testClose name actual expected closeness | abs (actual - expected) < closeness =
    do putStrLn $ "Test Case Passed: " ++ name
testClose name actual expected _ = 
    do putStrLn ""
       putStrLn $ "TEST CASE FAILED: " ++ name
       putStrLn $ "expected: " ++ show expected
       putStrLn $ "actual: " ++ show actual
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
          testRadiansToDegrees
          testDegreesToRadians
          testAngleAdd
          testAngleSubtract
          testDoubles
          testLoopedDoubles
          testConsecutives
          testShortConsecutives
          testLoopedConsecutives
          testShortLoopedConsecutives
          testAngleBetween
          testDistanceBetween
          testCrossProduct
          quickCheckCrossProductByAngleBetween
          testVectorAverage
          testNewell
          testDeterminant2
          testDeterminant3
          testDeterminant4
          quickCheckMatrixDeterminant2
          quickCheckMatrixDeterminant3
          quickCheckMatrixDeterminant4
          quickCheckMatrixMultiplyDeterminant2
          quickCheckMatrixMultiplyDeterminant3
          quickCheckMatrixMultiplyDeterminant4
          quickCheckMatrixInverse2
          quickCheckMatrixInverse3
          quickCheckMatrixInverse4