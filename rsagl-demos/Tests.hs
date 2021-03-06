{-# LANGUAGE Arrows #-}

--
-- Assorted test cases for RSAGL.
--

module Main
    (main)
    where

import RSAGL.FRP as FRP
import RSAGL.Math.Angle
import RSAGL.Math.Vector
import RSAGL.Math.Matrix
import RSAGL.Math.Types
import Control.Arrow
import Data.Set as Set
import Data.List as List
import Data.Monoid
import Test.QuickCheck
import RSAGL.Animation.Joint
import Data.Maybe
import Control.Monad
import RSAGL.Color
import RSAGL.FRP.Message

instance Arbitrary RSdouble where
    arbitrary = fmap f2f (arbitrary :: Gen Double)
    shrink = List.map f2f . (shrink :: Double -> [Double]) . f2f

instance Arbitrary RSfloat where
    arbitrary = fmap f2f (arbitrary :: Gen Float)
    shrink = List.map f2f . (shrink :: Float -> [Float]) . f2f

--
-- State machine that adds its input to its state
--
countingArrow :: (FRPModel m) => Integer ->
                                 FRP e
                                     (SimpleSwitch k t s i o m)
                                     Integer
                                     Integer
countingArrow x = summation x

--
-- State machine that is true iff the number of False inputs it has recieved is even
-- Could do this with 'accumulate' but using as a test 'switchTerminate' and 'switchContinue'.
--
evenZeroesArrow :: (FRPModel m) => FRP e (SimpleSwitch k () () Bool Bool m) Bool Bool
evenZeroesArrow = proc x ->
     do switchTerminate -< (if x then Nothing else Just evenZeroesArrow_oddZeroes,False)
        returnA -< True

evenZeroesArrow_oddZeroes :: (FRPModel m) => FRP e (SimpleSwitch k () () Bool Bool m) Bool Bool
evenZeroesArrow_oddZeroes = proc x ->
    do switchContinue -< (if x then Nothing else Just evenZeroesArrow,True)
       returnA -< False

--
-- A cellular automata that spawns the two adjacent cells (represented by
-- Integers) on each iteration.  Cells at non-zero integers divisible by 3 are
-- "sticky;" once reached they persist forever.  All other integers die out
-- after spawning.
--
spawnPlusAndMinusAndDie :: (FRPModel m,Capability k) =>
                           FRP e
                               (SimpleSwitch k () () () (Set Integer) m)
                               ()
                               (Set Integer)
spawnPlusAndMinusAndDie = frpContext forbidDuplicates [(0,step1)] >>>
                          arr (List.map snd >>> mconcat)
    where step1, step2 :: (FRPModel m) =>
              FRP e
                  (SimpleSwitch Enabled Integer () () (Set Integer) m)
                  ()
                  (Set Integer)
          step1 = proc () ->
              do i <- threadIdentity -< ()
                 switchTerminate -< (Just $ step2,Set.singleton i)
          step2 = proc () ->
              do i <- threadIdentity -< ()
                 spawnThreads -< [(i + 1,step1),(i - 1,step1)]
                 killThreadIf -< not $ i `mod` 3 == 0 && i /= 0
                 returnA -< Set.singleton i

--
-- Sanity test of ArrowState instance of FRP.
--
addFive :: Integer -> IO Integer
addFive x =
    do p <- newFRP1Program (countingArrow x)
       _ <- updateFRPProgram Nothing (1,()) p
       _ <- updateFRPProgram Nothing (3,()) p
       _ <- updateFRPProgram Nothing (-1,()) p
       _ <- updateFRPProgram Nothing (1,()) p
       liftM fst $ updateFRPProgram Nothing (1,()) p

--
--Sanity test of the switchTerminate and switchContinue functions.
--
evenZeroes :: [Bool] -> IO Bool
evenZeroes = liftM (head . last) . frpTest [evenZeroesArrow]

--
-- Sanity test of the ThreadedArrow
--
spawnPMD :: Int -> IO (Set Integer)
spawnPMD n = liftM (head . last) $ frpTest [spawnPlusAndMinusAndDie] $ replicate (n+1) ()

testIntegral :: IO ()
testIntegral = testCloseIO "testIntegral"
                  (frpTest [proc () -> integral (0 :: RSdouble) -< perSecond 1.0] (replicate 16 ()))
                  ([[0.0],[0.1],[0.2],[0.3],[0.4],[0.5],[0.6],[0.7],[0.8],[0.9],[1.0],[1.1],[1.2],[1.3],[1.4],[1.5]])
                  (listEqualClose $ listEqualClose $ equalClose)

testDerivative :: IO ()
testDerivative = testCloseIO "testDerivative"
                    (frpTest [derivative]
                             [5.0,6.0,8.0,11.0,15.0,20.0,26.0,33.0 :: RSdouble])
                    (List.map (List.map perSecond) [[0],[10],[20],[30],[40],[50],[60],[70]])
                    (==)

testInitial :: IO ()
testInitial = testCloseIO "testInitial"
                 (frpTest [initial]
                          [5,7,2,1,6,3,4])
                 [[5],[5],[5],[5],[5],[5],[5]]
                 (==)

testSticky :: IO ()
testSticky = testCloseIO "testSticky"
                  (frpTest [sticky odd 1]
                       [0,1,2,3,4,5,6,7,8,9,10,11])
                  [[1],[1],[1],[3],[3],[5],[5],[7],[7],[9],[9],[11]]
                  (==)

testRadiansToDegrees :: IO ()
testRadiansToDegrees = testClose "testRadiansToDegrees"
                          (toDegrees $ fromRadians (pi/6))
                          30
                          equalClose

testDegreesToRadians :: IO ()
testDegreesToRadians = testClose "testDegreesToRadians"
                          (toRadians $ fromDegrees 270)
                          (-pi/2)
                          equalClose

testAngleAdd :: IO ()
testAngleAdd = testClose "testAngleAdd"
                   (toDegrees $ fromDegrees 100 `angleAdd` fromDegrees 90)
                   (-170)
                   equalClose

testAngleSubtract :: IO ()
testAngleSubtract = testClose "testAngleSubtract"
                        (toDegrees $ fromDegrees (-20) `angleSubtract` fromDegrees 400)
                        (-60)
                        equalClose

testAngleBetween :: IO ()
testAngleBetween = testClose "testAngleBetween"
                   (toDegrees $ angleBetween (vector3d (-1,1,0)) (vector3d (0,0,1)))
                   90
                   equalClose

testDistanceBetween :: IO ()
testDistanceBetween = testClose "testDistanceBetween"
                      (distanceBetween (vector3d (-1,1,0)) (vector3d (0,0,1)))
                      (sqrt 3)
                      equalClose

--
-- We just test that the right-hand rule is observed
--
testCrossProduct :: IO ()
testCrossProduct =
    do let (x,y,z) = toXYZ $ crossProduct (vector3d (1,0,0)) (vector3d (0,1,0))
       testClose "testCrossProduct(x)" x 0.0 equalClose
       testClose "testCrossProduct(y)" y 0.0 equalClose
       testClose "testCrossProduct(z)" z 1.0 equalClose

--
-- crossProduct always yields a vector orthogonal to the parameters.
--
quickCheckCrossProductByAngleBetween :: IO ()
quickCheckCrossProductByAngleBetween = 
    do putStr "quickCheckCrossProductByAngleBetween: "
       quickCheck _qccpbab
           where _qccpbab (v1,v2) = let (a,b,c) = (vector3d v1,vector3d v2,crossProduct a b)
                                        in (toDegrees $ angleBetween a c) `equalClose` 90 &&
                                           (toDegrees $ angleBetween b c) `equalClose` 90

--
-- Orthos always yields two vectors orthogonal to the parameter.
--
quickCheckOrthos :: IO ()
quickCheckOrthos =
    do putStr "quickCheckOrthos: "
       quickCheck _qco
           where _qco v = let a = vector3d v
                              (b,c) = orthos a
                              in (toDegrees $ angleBetween a b) `equalClose` 90 &&
                                 (toDegrees $ angleBetween b c) `equalClose` 90 &&
                                 (toDegrees $ angleBetween a c) `equalClose` 90

--
-- Single test case for vectorAverage.
--
testVectorAverage :: IO ()
testVectorAverage =
    do let (x,y,z) = toXYZ $ vectorAverage [vector3d (0.1,0,0),vector3d (0,-2,0),vector3d (0,0,5)]
       testClose "testVectorAverage(x)" x 0.57735 equalClose
       testClose "testVectorAverage(y)" y (-0.57735) equalClose
       testClose "testVectorAverage(z)" z 0.57735 equalClose

--
-- Single test case for newell.
--
testNewell :: IO ()
testNewell =
    do let (x,y,z) = toXYZ $ fromMaybe (error "testNewell: Nothing") $ newell [point3d (1,0,0),point3d (0,1,0),point3d (0,0,-1)]
       testClose "testNewell(x)" x (-0.57735) equalClose
       testClose "testNewell(y)" y (-0.57735) equalClose
       testClose "testNewell(z)" z 0.57735 equalClose

-- An ad-hoc vector type.
type RSdouble4 = (RSdouble,RSdouble,RSdouble,RSdouble)
-- An ad-hoc matrix type.
type RSdouble44 = (RSdouble4,RSdouble4,RSdouble4,RSdouble4)

d4ToMatrix :: RSdouble44 -> Matrix
d4ToMatrix ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = matrix $ [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]

--
-- A single test of the determinant fuction.
--
testDeterminant4 :: IO ()
testDeterminant4 =
   do test "testDeterminant4-1"
           (determinant $ matrix [[1.5,-1.0,0.5,-2.0],[1.0,1.5,1.0,1.0],[1.5,1.0,0.5,-0.5],[1.0,1.5,1.0,0.0]])
           2

--
-- A single test of constructing an articulated joint.
--
testJoint :: IO ()
testJoint = testClose "testJoint"
    (joint_elbow $ joint (Vector3D 0 1 1) (Point3D 0 1 0) 3 (Point3D 0 0 1))
    (Point3D 0 1.43541 1.43541)
    xyzEqualClose

--
-- A matrix multiplied by the identity matrix should be itself.
--
quickCheckMatrixIdentity4 :: IO ()
quickCheckMatrixIdentity4 =
    do putStr "quickCheckMatrixIdentity4: "
       quickCheck _qcmi
           where _qcmi :: RSdouble44 -> Bool
                 _qcmi m = (identity_matrix `matrixMultiply` mat) `matrixEqualClose` mat
                     where mat = d4ToMatrix m

--
-- The determinant of a matrix is equal to the determinant of its transpose.
--
quickCheckMatrixDeterminant4 :: IO ()
quickCheckMatrixDeterminant4 =
    do putStr "quickCheckMatrixDeterminant4: "
       quickCheck _qcmi
           where _qcmi :: RSdouble44 -> Bool
                 _qcmi m =
                     determinant (matrixTransposePrim mat) `equalClose` determinant mat
                         where mat = d4ToMatrix m

--
-- The product of the determinants of two matrices is equal to the determinant
-- of the product of the two matrices.
--
quickCheckMatrixMultiplyDeterminant4 :: IO ()
quickCheckMatrixMultiplyDeterminant4 =
    do putStr "quickCheckMatrixMultiplyDeterminant4: "
       quickCheck _qcmmd
            where _qcmmd :: (RSdouble44,RSdouble44) -> Bool
                  _qcmmd (m1,m2) =
                       (determinant (mat1 `matrixMultiply` mat2)) `equalClose` (determinant mat1 * determinant mat2)
                           where mat1 = d4ToMatrix m1
                                 mat2 = d4ToMatrix m2

--
-- The inverse of the inverse of a matrix is equal to the original.
--
quickCheckMatrixInverse4 :: IO ()
quickCheckMatrixInverse4 =
    do putStr "quickCheckMatrixInverse4: "
       quickCheck _qcmi
           where _qcmi :: RSdouble44 -> Bool
                 _qcmi ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) =
                     if determinant mat `equalClose` 0
                     then True
                     else matrixInversePrim (matrixInversePrim mat) `matrixEqualClose` mat
                         where mat = matrix [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]

--
-- RSAGL matrices do considerable caching of inverses and transposes.  Test that
-- the cached values are consistent with the values derived from direct
-- manipulation.
--
quickCheckCachedMatrixValues :: IO ()
quickCheckCachedMatrixValues =
    do putStr "quickCheckCachedMatrixValues: "
       quickCheck _qccmv
           where _qccmv :: RSdouble44 -> Bool
                 _qccmv ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) =
                     let mat = matrix [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]
                         matrixAndDeterminantEqualClose x y = determinant x `equalClose` determinant y && x `matrixEqualClose` y
                         sensible x = matrixInverse x `matrixAndDeterminantEqualClose` matrixInversePrim x &&
                                      matrixTranspose x `matrixAndDeterminantEqualClose` matrixTransposePrim x &&
                                      (matrixInverse . matrixTranspose) x `matrixAndDeterminantEqualClose` (matrixInversePrim . matrixTransposePrim) x &&
                                      (matrixTranspose . matrixInverse) x `matrixAndDeterminantEqualClose` (matrixTransposePrim . matrixInversePrim) x
                         in if determinant mat `equalClose` 0 then True
                                else all sensible [mat,matrixInverse mat,matrixTranspose mat,matrixInverse $ matrixTranspose mat,matrixTranspose $ matrixInverse mat]

--
-- Tests taking the maximal value of a color system.
--
quickCheckColorMaximals :: IO ()
quickCheckColorMaximals =
    do putStr "quickCheckColorMaximals: "
       quickCheck _qccm
           where _qccm :: (RSdouble,RSdouble,RSdouble) -> Bool
                 _qccm (r,g,b) = ((maximum [r,g,b]
                     `equalClose`
                         (linear_value $ viewChannel channel_value $ RGB r g b))
                     &&
                         ((1-minimum [r,g,b])
                     `equalClose`
                         (linear_value $ viewChannel channel_boldness $ RGB r g b)))
                     ||
                         (r > 1 || r < 0 || g > 1 || g < 0 || b > 1 || b < 0)

--
-- Tests maximizing an RGB color.
--
testMaximizeRGB :: IO ()
testMaximizeRGB = testClose "testMaximizeRGB"
                      (adjustColor channel_value maximize $ RGB 0.25 0.1 0.5)
                      (RGB 0.5 0.2 1.0)
                      xyzEqualClose

--
-- Tests measuring the luminance of a color.
--
quickCheckMeasureLuminance :: IO ()
quickCheckMeasureLuminance =
    do putStr "quickCheckMeasureLuminance: "
       quickCheck _qcml
  where _qcml :: (RSdouble,RSdouble,RSdouble) -> Bool
        _qcml (r,g,b) =
            let luminance = (r*0.2126 + g*0.7152 + b*0.0722)
                in luminance `equalClose` (linear_value $ viewChannel channel_luminance $ RGB r g b) &&
                   luminance `equalClose` hcl_luminance (transformColor $ RGB r g b)

--
-- Tests maximizing the amount of blue in a color.
--
quickCheckAdjustToBlue :: IO ()
quickCheckAdjustToBlue =
    do putStr "quickCheckAdjustToBlue: "
       quickCheck _qcatb
  where _qcatb :: (RSdouble,RSdouble,RSdouble) -> Bool
        _qcatb (r,g,b) =
            let (RGB r' g' b') = adjustColor channel_blue_yellow
                                             ((+0.1) . linear_value)
                                             (RGB r g b)
                in abs (((r'+g')/2+b') - ((r+g)/2+b)) < 1.0e-4 &&
                       b' >= b && r' <= r && g' <= g

--
-- Single test of the RK4 algorithm.
--
testRK4 :: IO ()
testRK4 = testClose "testRK4"
                    (integrateRK4 (+) (\t _ -> perSecond $ cos $ toSeconds t) 0
                                      (fromSeconds 0) (fromSeconds 1) 100)
                    (sin 1.0 :: RSdouble)
                    equalClose

--
-- Simple tests of the FRP.Message algorithm.
--
testMessages :: IO ()
testMessages =
    do t <- newTransmitterBy $ \a b -> fst a == fst b
       r <- newReceiver
       f <- send $ \(n,s) -> (n, "Test case " ++ s)
       print . snd =<< receive r . (f <<*>>) =<< transmit t (1,"Passed: testMessages(1)")
       receive r . (f <<*>>) =<< transmit t (2,"Passed: testMessages(2)")
       print . snd =<< receive r . (f <<*>>) =<< transmit t (2,"FAILED: testMessages(2)")
       print . snd =<< receive r . (f <<*>>) =<< transmit t (3,"Passed: testMessages(3)")

--
-- Test harness.
--
test :: (Eq a,Show a) => String -> a -> a -> IO ()
test name actual expected | actual == expected =
                       do putStrLn $ "Test Case Passed: " ++ name
test name actual expected =
                       do putStrLn ""
                          putStrLn $ "TEST CASE FAILED: " ++ name
                          putStrLn $ "expected: " ++ show expected
                          putStrLn $ "actual:   " ++ show actual
                          putStrLn ""

--
-- Test harness for IO actions.
--
testIO :: (Eq a,Show a) => String -> IO a -> a -> IO ()
testIO name actualIO expected =
    do putStrLn $ "Running IO: " ++ name
       actual <- actualIO
       test name actual expected

--
-- Test approximate equality.
--
equalClose :: (Eq a,Num a,Ord a,Fractional a,Floating a) => a -> a -> Bool
equalClose actual expected | actual == expected = True
equalClose actual expected | signum actual /= signum expected = False
equalClose 0 0 = True
equalClose _ 0 = False
equalClose 0 _ = False
equalClose actual expected = abs (log (abs actual) - log (abs expected)) < 0.01

--
-- Test approximate equality with lists.
--
listEqualClose :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEqualClose f xs ys | length xs == length ys = and $ zipWith f xs ys
listEqualClose _ _ _ = False

--
-- Test approximate equality with matrices.
--
matrixEqualClose :: Matrix -> Matrix -> Bool
matrixEqualClose m n = and $ List.map and $ zipWith (zipWith equalClose)
    (rowMajorForm m) (rowMajorForm n)

--
-- Test approximate equality with 3-tuples.
--
xyzEqualClose :: (Xyz xyz) => xyz -> xyz -> Bool
xyzEqualClose a b = equalClose ax bx && equalClose ay by && equalClose az bz
    where (ax,ay,az) = toXYZ a
          (bx,by,bz) = toXYZ b

--
-- Test harness using an approximate equality test.
--
testClose :: (Show a) => String -> a -> a -> (a -> a -> Bool) -> IO ()
testClose name actual expected f | f actual expected =
    do putStrLn $ "Test Case Passed: " ++ name
testClose name actual expected _ =
    do putStrLn ""
       putStrLn $ "TEST CASE FAILED: " ++ name
       putStrLn $ "expected: " ++ show expected
       putStrLn $ "actual: " ++ show actual
       putStrLn ""

--
-- Test harness using an approximate equality test using an IO action.
--
testCloseIO :: (Show a) => String -> IO a -> a -> (a -> a -> Bool) -> IO ()
testCloseIO name actualIO expected f =
    do putStrLn $ "Running IO: " ++ name
       actual <- actualIO
       testClose name actual expected f

main :: IO ()
main = do testIO "add five test (sanity test of accumulation)"
               (addFive 2) 7
          testIO "even zeroes test (sanity test of switching)"
               (evenZeroes [True,True,False,True,False]) True
          testIO "odd zeroes test (sanity test of switching)"
               (evenZeroes [True,True,True,False,False,True,False,True]) False
          testIO "spawning test 1 (sanity test of threading)"
               (spawnPMD 0) (Set.fromList [0])
          testIO "spawning test 2 (sanity test of threading)"
               (spawnPMD 1) (Set.fromList [-1,1])
          testIO "spawning test 3 (sanity test of threading)"
               (spawnPMD 2) (Set.fromList [0,-2,2])
          testIO "spawning test 4 (sanity test of threading)"
               (spawnPMD 3) (Set.fromList [-3,-1,1,3])
          testIO "spawning test 5 (does killThreadIf work conditionally?)"
               (spawnPMD 4) (Set.fromList [-4,-3,-2,0,2,3,4])
          testIntegral
          testDerivative
          testInitial
          testSticky
          testRadiansToDegrees
          testDegreesToRadians
          testAngleAdd
          testAngleSubtract
          testAngleBetween
          testDistanceBetween
          testCrossProduct
          quickCheckCrossProductByAngleBetween
          quickCheckOrthos
          testVectorAverage
          testNewell
          testDeterminant4
          quickCheckMatrixIdentity4
          quickCheckMatrixDeterminant4
          quickCheckMatrixMultiplyDeterminant4
          quickCheckMatrixInverse4
          quickCheckCachedMatrixValues
          testJoint
          testRK4
          quickCheckColorMaximals
          quickCheckMeasureLuminance
          quickCheckAdjustToBlue
          testMaximizeRGB
          testMessages
