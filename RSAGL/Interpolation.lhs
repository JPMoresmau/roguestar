\begin{code}
module RSAGL.Interpolation
    (Lerpable(..),
     genericLerp,
     lerpBetween,
     lerpBetweenMutated,
     lerpBetweenClamped,
     lerpBetweenClampedMutated,
     lerp_mutator_continuous_1st,
     lerpMap,
     interpolateGrid,
     interpolate3DPolyline,
     interpolateLooped3DPolyline,
     interpolate2DPolyline)
    where

import RSAGL.Angle
import RSAGL.Vector
import Data.List
import Control.Monad
import RSAGL.ListUtils

-- |
-- Class of things subject to linear interpolation.
--
-- lerp u a b, with u at or between zero and unity according to its type,
-- is an entity some fraction of the distance between a b, with u=0
-- indicating a and u=1 indicating b.  If reasonable for the types
-- involved, u may be outside of the zero and unity, with a negative
-- u indicating a point on the opposite side of a from b and with
-- a u > 1 indicating a point on the opposite side of b from a.
--
class Lerpable a where
    lerp :: Double -> (a,a) -> a

instance (Lerpable a,Lerpable b) => Lerpable (a,b) where
    lerp u ((a1,a2),(b1,b2)) = (lerp u (a1,b1),lerp u (a2,b2))

genericLerp :: (Lerpable a,Real r) => r -> (a,a) -> a
genericLerp u (a,b) = lerp (realToFrac u) (a,b)

-- |
-- lerp takes a parameter between 0 and 1, while lerpBetween takes a parameter between two arbitrary values.
-- lerp u (a,b) == lerpBetween (0,u,1) (a,b)
--
lerpBetween :: (Lerpable a,Real r,Fractional r) => (r,r,r) -> (a,a) -> a
lerpBetween = lerpBetweenMutated id

lerpBetweenMutated :: (Lerpable a,Real r,Fractional r) => (Double -> Double) -> (r,r,r) -> (a,a) -> a
lerpBetweenMutated mutator (l,u,r) = lerp $ mutator $ realToFrac $ (u-l) / (r-l)

-- |
-- As lerpBetween, but constrains the parameter to the range 0 <= u <= 1.
--
lerpBetweenClamped :: (Lerpable a,Real r,Fractional r,Ord r) => (r,r,r) -> (a,a) -> a
lerpBetweenClamped = lerpBetweenClampedMutated id

lerpBetweenClampedMutated :: (Lerpable a,Real r,Fractional r,Ord r) => (Double -> Double) -> (r,r,r) -> (a,a) -> a
lerpBetweenClampedMutated mutator (l,u,r) = lerp $ max 0 $ min 1 $ mutator $ realToFrac $ (u-l) / (r-l)

-- |
-- A lerp mutator that gives a continuous 1st derivitive to the entity's change over the parameter,
-- assuming it had continuous change to begin with.
--
lerp_mutator_continuous_1st :: Double -> Double
lerp_mutator_continuous_1st x | x < 0 = 0
lerp_mutator_continuous_1st x | x > 1 = 1
lerp_mutator_continuous_1st x | x <= 0.5 = (x*2)^2 / 2
lerp_mutator_continuous_1st x = 1 - ((1-x)*2)^2 / 2

-- |
-- Given many entities, lerp between the two entities closest to the given point
-- on either side.  For example, if we wanted to lerp between colors on a rainbow,
-- we might use the map [(0,RED),(1,ORANGE),(2,YELLOW),(3,GREEN),(4,BLUE),(5,INDIGO),(6,VIOLET)].
-- lerpMap 3.5 would result in a blue-green color.
--
lerpMap :: (Lerpable a) => Double -> [(Double,a)] -> a
lerpMap u pts = 
    let (l,l') = minimumBy (\x -> \y -> compare (fst x) (fst y)) $ filter ((>= u) . fst) pts
        (r,r') = maximumBy (\x -> \y -> compare (fst x) (fst y)) $ filter ((<= u) . fst) pts
        in lerpBetween (l,u,r) (l',r')
                
lerpNumber :: (Num n) => n -> (n,n) -> n
lerpNumber u (a,b) = (1-u)*a + u*b
\end{code}

\subsection{Lerpable entities from RSAGL}

\begin{code}
instance Lerpable Double where
   lerp = lerpNumber

instance Lerpable Vector3D where
   lerp u (Vector3D ax ay az,Vector3D bx by bz) = Vector3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point3D where
   lerp u (Point3D ax ay az,Point3D bx by bz) = Point3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point2D where
   lerp u (Point2D ax ay,Point2D bx by) = Point2D (lerp u (ax,bx)) (lerp u (ay,by))
                
instance (Lerpable a) => Lerpable (Maybe a) where
   lerp u (a,b) = liftM2 (curry $ lerp u) a b

instance Lerpable Angle where
    lerp u (a,b) = a + scaleAngle u (b - a)
\end{code}

\subsection{Interpolating geometric data}

interpolateGrid uses a simple interpolation function to smooth out a grid of 3D vertex data.

We hereby define that each sublist of the vertex data represents a polyline of longitude, and that lines drawn between lists represent polylines of latitude.

The boolean parameters indicate whether or not longitudinal and latitudinal lines are looped polylines.  For looped polylines, an extra point is interpolated between the first and last points in the polyline.

\begin{code}
interpolateGrid :: (Bool,Bool) -> [[Point3D]] -> [[Point3D]]
interpolateGrid (loop_long,loop_lat) = transpose . map (interpolateFn loop_lat) . transpose . map (interpolateFn loop_long)
    where interpolateFn True = interpolateLooped3DPolyline
          interpolateFn False = interpolate3DPolyline
\end{code}

The following functions interpolate polylines to smooth them.  The goal here is to be as true as possible to the original
polyline while making it appear to be smoother.

\begin{code}
interpolate3DPolyline :: [Point3D] -> [Point3D]
interpolate3DPolyline pts | length pts < 4 = head pts :
					    (concatMap 
					     (\(p1,p2) -> [lerp 0.5 (p1,p2),p2]) $ 
					    doubles pts)
interpolate3DPolyline pts = let len = length pts
			        begin = take 3 pts
			        end = drop (len-2) pts
			        in (take 3 $ interpolate3DPolyline begin) ++ 
				       (concatMap (\x -> [interpolate4 x,x !! 2]) $ consecutives 4 pts) ++ 
				       (drop 2 $ interpolate3DPolyline end)

interpolateLooped3DPolyline :: [Point3D] -> [Point3D]
interpolateLooped3DPolyline pts | length pts < 4 = (concatMap
						   (\(p1,p2) -> [lerp 0.5 (p1,p2),p2]) $
						  loopedDoubles pts)
interpolateLooped3DPolyline pts = (concatMap (\x -> [interpolate4 x,x !! 2]) $ loopedConsecutives 4 pts)

interpolate2DPolyline :: [Point2D] -> [Point2D]
interpolate2DPolyline = map to2d . interpolate3DPolyline . map to3d

interpolate4 :: [Point3D] -> Point3D
interpolate4 [p0,p1,p2,p3] = 
    let (Point3D x y z) = lerp (0.5 :: Double) (p1,p2)
	simple = vectorAdd (vectorToFrom p1 p0) (vectorToFrom p2 p3)
	scaled = maybe (Vector3D 0 0 0) (vectorScaleTo (vectorLength (vectorToFrom p1 p2) / 9)) $ aNonZeroVector simple
	(Vector3D x' y' z') = if (vectorLength simple < vectorLength scaled)
			      then simple
			      else scaled
	in Point3D (x+x'/2) (y+y'/2) (z+z'/2)
interpolate4 _ = error "interpolate4: works only on lists of 4"
\end{code}