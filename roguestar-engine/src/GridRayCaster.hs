
module GridRayCaster
    (castRays,
     castRay,
     gridRayCasterTests)
    where

import Data.Set as Set
import Data.List as List
import Data.Ratio
import Tests

-- |
-- When casting large numbers of rays from the same point, castRays will try to do this in
-- O( n^2 ), although O( n^3 ) is still the worst case.  It does cheat a little.
--
castRays :: (Integer,Integer) -> [((Integer,Integer),Integer)] -> ((Integer,Integer) -> Integer) -> [(Integer,Integer)]
castRays src@(src_x,src_y) dests opacityFn =
    toList $
    foldr (\ l m -> Set.union m $ fromList $ castRays_ Nothing m l) empty $ -- cast the rays, acumulating the already cast rays into a map and passing it into the next castRay_ where it will be used to cheat
    sortBy (\ a b -> lengthThenDistance a b) $  -- sort the groups so that the largest groups are on the right, in case of equal lengths, move groups with the most distant member to the right (to exploit more cases where we can cheat)
    List.map (sortBy compareDistance) $ -- sort each group by distance, so the most distant ones come first (then we'll skip the nearer ones if the more distant passes and the nearer is brighter)
    groupBy (\ a b -> compareDirection a b == EQ) $ -- order and group the all destinations that lie along the same ray
    sortBy (\ a b -> compareDirection a b) dests
	where lengthThenDistance a b = case (length a) `compare` (length b) of
									    EQ -> (head b) `compareDistance` (head a)
									    ordering -> ordering
	      compareDistance ((x1,y1),_) ((x2,y2),_) = compare (abs (x2-src_x) + abs (y2-src_y)) (abs (x1-src_x) + abs (y1-src_y)) -- pairs 1 and 2 deliberately reversed to get reverse sort order
	      compareDirection ((x1,y1),_) ((x2,y2),_) | (src_y - y1 == 0) && (src_y - y2 == 0) = signum (src_x-x1) `compare` signum (src_x-x2)
	      compareDirection ((_,y1),_) _ | (src_y - y1 == 0) = LT
	      compareDirection _ ((_,y2),_) | (src_y - y2 == 0) = GT
	      compareDirection ((x1,y1),_) ((x2,y2),_) = 
		  let slope1 = (src_x-x1)%(src_y-y1) 
		      slope2 = (src_x-x2)%(src_y-y2)
		      in case slope1 `compare` slope2 of
						      EQ -> signum (src_y-y1) `compare` signum (src_y-y2)
						      ordering -> ordering
	      castRays_ _ _ [] = []
	      -- in this case: if a more distant ray from a darker spot passes, then the nearer, brighter ray obviously passes (NOT cheating!)
	      castRays_ (Just old_brightness) m ((dest,brightness):rest) | brightness >= old_brightness = dest : (castRays_ (Just old_brightness) m rest)
	      -- in this case: if two of the three spots near to this spot, but one step further from the observer, pass, then pass this spot (cheating!)
	      castRays_ maybe_old_brightness m (((dx,dy),_):rest) | (>= 2) $ length $ List.filter (flip member m) [(dx+signum (dx-src_x),dy),(dx,dy+signum (dy-src_y)),(dx+signum (dx-src_x),dy+signum (dy-src_y))] = (dx,dy) : (castRays_ maybe_old_brightness m rest)
	      -- if we don't have a basis to automatically include this spot, then actually cast a ray (expensive!)
	      castRays_ maybe_old_brightness m ((dest,brightness):rest) = if castRay src dest brightness opacityFn
									  then dest : (castRays_ (Just brightness) m rest)
									  else castRays_ maybe_old_brightness m rest

-- |
-- Facade function to castRayForOpacity.
--
castRay :: (Integer,Integer) -> (Integer,Integer) -> Integer -> ((Integer,Integer) -> Integer) -> Bool
castRay (ax,ay) (bx,by) brightness opacityFn =
    castRayForOpacity     (1/8)
			  (fromInteger ax,fromInteger ay)
			  (fromInteger bx,fromInteger by)
			  (fromInteger brightness)
			  (integerToFloatOpacityGrid opacityFn)

data Ray = Ray { ray_origin :: !(Float,Float),
		 ray_delta :: !(Float,Float) }

integerToFloatOpacityGrid :: ((Integer,Integer) -> Integer) -> ((Float,Float) -> Float)
integerToFloatOpacityGrid fn (x,y) =
  let x_ceil = ceiling x
      x_floor = floor x
      y_ceil = ceiling y
      y_floor = floor y
      x_part = x - (fromInteger $ floor x)
      y_part = y - (fromInteger $ floor y)
      x_part_inv = 1 - x_part
      y_part_inv = 1 - y_part
      cc = fromInteger $ fn (x_ceil,y_ceil)
      cf = fromInteger $ fn (x_ceil,y_floor)
      fc = fromInteger $ fn (x_floor,y_ceil)
      ff = fromInteger $ fn (x_floor,y_floor)
      in x_part     * y_part     * ff +
         x_part_inv * y_part     * cf +
         x_part     * y_part_inv * fc +
         x_part_inv * y_part_inv * cc


-- |
-- Cast a ray from point a to b, through a medium with variable opacity.
--
-- A ray passes if it ends with a brightness greater than 1.
--
castRayForOpacity :: Float -> (Float,Float) -> (Float,Float) -> Float -> ((Float,Float)->Float) -> Bool
castRayForOpacity fineness a@(ax,ay) b@(bx,by) brightness rawOpacityFn =
    let ray = setRayLength fineness $ rayFromTo a b
	opacityFn = \ x -> (1 - rawOpacityFn x / 100) ** fineness
	lengthSquared (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2
	goal_length = minimum $ List.map (lengthSquared a) [(bx - signum (bx-ax),by),(bx,by - signum (by-ay)),(bx - signum (bx-ax),by + signum (by-ay))]
	in all (> 1) $
	   scanl (\ bright pt -> bright * opacityFn pt) brightness $
	   takeWhile ( \ pt -> lengthSquared a pt < goal_length) $
	   rayToPoints ray

-- |
-- Generates a ray from the first point through the second point.
--
rayFromTo :: (Float,Float) -> (Float,Float) -> Ray
rayFromTo (ax,ay) (bx,by) = Ray (ax,ay) (bx-ax,by-ay)

-- |
-- Sets the length of the ray's delta.
--
setRayLength :: Float -> Ray -> Ray
setRayLength new_distance ray@(Ray { ray_delta=(dx,dy) }) = 
    let old_distance = sqrt $ (dx^2 + dy^2)
	scalar = new_distance/old_distance
	in ray { ray_delta=(scalar*dx,scalar*dy) }

-- |
-- Advances a ray by its ray_delta.
--
incrementRay :: Ray -> Ray
incrementRay ray@(Ray {ray_origin=(ax,ay), ray_delta=(dx,dy)}) =
    ray { ray_origin=(ax+dx,ay+dy) }

-- |
-- Transforms a ray from point a to point b into a stream of points,
-- beginning with the origin of the ray.
--
rayToPoints :: Ray -> [(Float,Float)]
rayToPoints ray = List.map ray_origin $ iterate (incrementRay) ray

sampleDensityFunction :: (Integer,Integer) -> Integer
sampleDensityFunction (x,y) = (abs x + abs y)

gridRayCasterTests :: [TestCase]
gridRayCasterTests = [easyRayTest,hardRayTest,tooHardRayTest,stressLazyRayTest]

easyRayTest :: TestCase
easyRayTest = (if castRay (4,5) (-3,-1) 100 sampleDensityFunction
	       then return (Passed "easyRayTest")
	       else return (Failed "easyRayTest"))

hardRayTest :: TestCase
hardRayTest = (if castRay (10,0) (0,10) 5 sampleDensityFunction
	       then return (Passed "hardRayTest")
	       else return (Failed "hardRayTest"))

tooHardRayTest :: TestCase
tooHardRayTest = (if castRay (10,0) (0,10) 4 sampleDensityFunction
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
