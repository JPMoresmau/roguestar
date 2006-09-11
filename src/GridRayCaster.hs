module GridRayCaster
    (castRay,
     gridRayCasterTests)
    where

import Data.Ratio
import Tests

-- |
-- Facade function to castRayForOpacity.
-- Casts a ray with a brightness of 5 from point a to point b through a medium defined
-- by opacityFn.
--
-- castRay a b 5 opacityFn
--
castRay :: (Integer,Integer) -> (Integer,Integer) -> Integer -> ((Integer,Integer) -> Integer) -> Bool
castRay (ax,ay) (bx,by) brightness opacityFn =
    castRayForOpacity     (1%10)
			  (ax%1,ay%1)
			  (bx%1,by%1)
			  (brightness%1)
			  (integerToRationalOpacityGrid opacityFn)

data Ray = Ray { ray_origin :: (Rational,Rational),
		 ray_delta :: (Rational,Rational) }

integerToRationalOpacityGrid :: ((Integer,Integer) -> Integer) -> ((Rational,Rational) -> Rational)
integerToRationalOpacityGrid fn (x,y) =
    let partx = abs $ x - (floor x % 1)
	party = abs $ y - (floor y % 1)
	partx_inv = 1 - partx
	party_inv = 1 - party
	ff = (fn (floor x,floor y) % 1) * partx * party
	fc = (fn (floor x,ceiling y) % 1) * partx * party_inv
	cf = (fn (ceiling x, floor y) % 1) * partx_inv * party
	cc = (fn (ceiling x, ceiling y) % 1) * partx_inv * party_inv
	in ff + fc + cf + cc

-- |
-- Cast a ray from point a to b, through a medium with variable opacity defined by opacityFn, 
-- determining whether or not light from point a will reach point b.
--
-- Opacity is relative to the brightness of a ray -- a unit-square region of material with an opacity of
-- 5 will completely block a ray with a brightness of 5.  A region with an opacity of 1, however,
-- will block 1/5th of the brightness of the ray each time the ray passes through a unit square with that
-- brightness.
--
-- If a ray ends with a brightness less than 1, then is considered completely blocked, otherwise it is
-- considered to have passed.
--
castRayForOpacity :: Rational -> (Rational,Rational) -> (Rational,Rational) -> Rational -> ((Rational,Rational)->Rational) -> Bool
castRayForOpacity fineness a b brightness rawOpacityFn =
    let ray = setRayLength fineness $ rayFromTo a b
	opacityFn = \ x -> 1 - rawOpacityFn x * fineness / brightness
	lengthSquared (ax,ay) (bx,by) = (ax-bx)^2 + (ay-by)^2
	goal_length = lengthSquared a b
	in all (> 1) $ 
	   scanl (\ bright pt -> bright * opacityFn pt) brightness $ 
	   takeWhile (\ pt -> lengthSquared a pt < goal_length) $
	   rayToPoints ray

-- |
-- Generates a ray from the first point through the second point.
--
rayFromTo :: (Rational,Rational) -> (Rational,Rational) -> Ray
rayFromTo (ax,ay) (bx,by) = Ray (ax,ay) (bx-ax,by-ay)

-- |
-- Sets the length of the ray's delta.
--
setRayLength :: Rational -> Ray -> Ray
setRayLength new_distance ray@(Ray { ray_delta=(dx,dy) }) = 
    let old_distance = toRational . sqrt . fromRational $ (dx^2 + dy^2)
	scalar = new_distance/old_distance
	in ray { ray_delta=(scalar*dx,scalar*dy) }

-- |
-- Moves a ray by its ray_delta.
--
incrementRay :: Ray -> Ray
incrementRay ray@(Ray {ray_origin=(ax,ay), ray_delta=(dx,dy)}) =
    ray { ray_origin=(ax+dx,ay+dy) }

-- |
-- Transforms a ray from point a to point b into a list of points,
-- where seglength is the distance to each subsequent point,
-- such that all points lie on the ray.  The result is an infinite
-- list.
rayToPoints :: Ray -> [(Rational,Rational)]
rayToPoints ray = ray_origin ray : rayToPoints (incrementRay ray)

sampleDensityFunction :: (Integer,Integer) -> Integer
sampleDensityFunction (x,y) = (abs x + abs y) `mod` 10

gridRayCasterTests :: [TestCase]
gridRayCasterTests = [easyRayTest,hardRayTest,tooHardRayTest,stressLazyRayTest]

easyRayTest :: TestCase
easyRayTest = (if castRay (4,5) (-3,-1) 100 sampleDensityFunction
	       then return (Passed "easyRayTest")
	       else return (Failed "easyRayTest"))

hardRayTest :: TestCase
hardRayTest = (if castRay (0,25) (25,0) (25^2+1) sampleDensityFunction
	       then return (Passed "hardRayTest")
	       else return (Failed "hardRayTest"))

tooHardRayTest :: TestCase
tooHardRayTest = (if castRay (0,10) (0,-10) 10 sampleDensityFunction
		  then return (Failed "tooHardRayTest")
		  else return (Passed "tooHardRayTest"))

-- |
-- This test should evaluate quickly, even though the ray is very long, because the ray
-- will be opaqued early the casting of the ray.
--
stressLazyRayTest :: TestCase
stressLazyRayTest = (if castRay (-1,0) (1,2500000) 2 sampleDensityFunction
		     then return (Failed "stressLazyRayTest")
		     else return (Passed "stressLazyRayTest"))