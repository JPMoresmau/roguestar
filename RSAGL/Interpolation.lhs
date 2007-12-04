\section{RSAGL.Interpolation}

\begin{code}
module RSAGL.Interpolation
    (Lerpable(..),
     genericLerp,
     lerpNumber,
     lerpBetween,
     lerpBetweenMutated,
     lerpBetweenClamped,
     lerpBetweenClampedMutated,
     lerp_mutator_continuous_1st,
     lerpMap)
    where

import Control.Applicative
import RSAGL.ApplicativeWrapper
import RSAGL.Angle
import RSAGL.Vector
import Data.List
\end{code}

\subsection{The Lerpable typeclass}

Lerpable data types are subject to linear interpolation.

The lerp function takes a $u$-value and two endpoints, $a$ and $b$.  If $u$ lies between 0 and 1, then the result lies between $a$ and $b$.
If $u = 0$ or $u = 1$, then the result is $a$ or $b$, respectively.  If $u < 0$, then the result is on the opposite side of $a$ from $b$.  If $u > 1$, then the result is on the opposite side of $b$ from $a$.

\begin{code}
class Lerpable a where
    lerp :: Double -> (a,a) -> a

instance (Lerpable a,Lerpable b) => Lerpable (a,b) where
    lerp u ((a1,a2),(b1,b2)) = (lerp u (a1,b1),lerp u (a2,b2))

genericLerp :: (Lerpable a,Real r) => r -> (a,a) -> a
genericLerp u (a,b) = lerp (realToFrac u) (a,b)
\end{code}

\subsection{Non-linear interpolations}

The mutated versions of the lerp functions takes an arbitrary ``mutator function'' to define non-linear interpolation curves.
The ``between'' versions of the lerp functions allow the u-value to lie between any two numbers, as opposed to between 0 and 1.
The ``clamped'' versions of the lerp functions clamp the u-value to lie between its boundaries.  Otherwise, with non-clamped
interpolations, the u-value may lie outside of its boundaries.

\begin{code}
lerpBetween :: (Lerpable a,Real r,Fractional r) => (r,r,r) -> (a,a) -> a
lerpBetween = lerpBetweenMutated id

lerpBetweenMutated :: (Lerpable a,Real r,Fractional r) => (Double -> Double) -> (r,r,r) -> (a,a) -> a
lerpBetweenMutated _ (l,_,r) | l == r = lerp 0.5
lerpBetweenMutated mutator (l,u,r) = lerp $ mutator $ realToFrac $ (u-l) / (r-l)

lerpBetweenClamped :: (Lerpable a,Real r,Fractional r,Ord r) => (r,r,r) -> (a,a) -> a
lerpBetweenClamped = lerpBetweenClampedMutated id

lerpBetweenClampedMutated :: (Lerpable a,Real r,Fractional r,Ord r) => (Double -> Double) -> (r,r,r) -> (a,a) -> a
lerpBetweenClampedMutated mutator (l,u,r) = lerpBetweenMutated (lerp_mutator_clamp . mutator) (l,u,r)
\end{code}

\subsection{Lerp mutators}

\texttt{lerp_mutator_clamp} implements clamping between 0 and 1.

\begin{code}
lerp_mutator_clamp :: Double -> Double
lerp_mutator_clamp = min 1.0 . max 0.0
\end{code}

\texttt{lerp_mutator_continuous_1st} implements clamping between 0 and 1, but such that the 1st derivative of the result is continuous.

\begin{code}
lerp_mutator_continuous_1st :: Double -> Double
lerp_mutator_continuous_1st x | x < 0 = 0
lerp_mutator_continuous_1st x | x > 1 = 1
lerp_mutator_continuous_1st x | x <= 0.5 = 2*x^2
lerp_mutator_continuous_1st x = 4*x - 2*x^2 - 1
\end{code}

\subsection{lerpMap}

Given many entities, lerp between the two entities closest to the given point
on either side.  For example, if we wanted to lerp between colors on a rainbow,
we might use the map [(0,red),(1,orange),(2,yellow),(3,green),(4,blue),(5,indigo),(6,violet)].
lerpMap 3.5 would result in a blue-green color.

\begin{code}
lerpMap :: (Lerpable a) => [(Double,a)] -> Double -> a
lerpMap pts u = 
    let (l,l') = minimumBy (\x y -> compare (fst x) (fst y)) $ filter ((>= u) . fst) pts
        (r,r') = maximumBy (\x y -> compare (fst x) (fst y)) $ filter ((<= u) . fst) pts
        in lerpBetween (l,u,r) (l',r')

{-# SPECIALISE INLINE lerpNumber :: Double -> (Double,Double) -> Double #-}
{-# SPECIALISE INLINE lerpNumber :: Float -> (Float,Float) -> Float #-}
lerpNumber :: (Num n) => n -> (n,n) -> n
lerpNumber u (a,b) = a + u*(b - a)

lerpA :: (Applicative f,Lerpable a) => f Double -> (f a,f a) -> f a
lerpA u (a,b) = (\u' -> curry (lerp u')) <$> u <*> a <*> b

lerpAPure :: (Applicative f,Lerpable a) => Double -> (f a,f a) -> f a
lerpAPure u = lerpA (pure u)
\end{code}

\subsection{Lerpable data structures}

\begin{code}
instance Lerpable Double where
   lerp = lerpNumber

instance Lerpable Vector3D where
   lerp u (Vector3D ax ay az,Vector3D bx by bz) = Vector3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point3D where
   lerp u (Point3D ax ay az,Point3D bx by bz) = Point3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point2D where
   lerp u (Point2D ax ay,Point2D bx by) = Point2D (lerp u (ax,bx)) (lerp u (ay,by))

instance Lerpable Angle where
    lerp u (a,b) = a + scaleAngle u (b - a)

instance (Lerpable a) => Lerpable (Maybe a) where
   lerp = lerpAPure

instance (Lerpable a) => Lerpable ((->) r a) where
    lerp = lerpAPure

instance (Lerpable a,Applicative f) => Lerpable (ApplicativeWrapper f a) where
    lerp = lerpAPure
\end{code}

\subsection{Resampling geometric data}

resample3DSurface uses a simple interpolation function to smooth out a grid of 3D vertex data.

We hereby define that each sublist of the vertex data represents a polyline of longitude, and that lines drawn between lists represent polylines of latitude.

The boolean parameters indicate whether or not longitudinal and latitudinal lines are looped polylines.  For looped polylines, an extra point is interpolated between the first and last points in the polyline.

begin{code}
resample3DSurface :: (Bool,Bool) -> (Integer,Integer) -> [[Point3D]] -> [[Point3D]]
resample3DSurface (loop_long,loop_lat) (n_long,n_lat) = 
    transpose . map (resampleFn loop_lat n_lat) . transpose . map (resampleFn loop_long n_long)
        where resampleFn True = resampleLooped3DPolyline
              resampleFn False = resample3DPolyline
\end{code}

resample2DPolyline and resample3DPolyline assume that a polyline approximates a smooth curve and redefine the polyline in more or fewer points.  This is a lossy operation.

\footnote{FIXME: resamplePolyline runs in $O\(2^n\)$ time due to iterated calls to lerpMap; this could be improved}

begin{code}
resample2DPolyline :: Integer -> [Point2D] -> [Point2D]
resample2DPolyline n pts = resamplePolyline interpolate2DPolyline n pts

resample3DPolyline :: Integer -> [Point3D] -> [Point3D]
resample3DPolyline n pts = resamplePolyline interpolate3DPolyline n pts

resampleLooped3DPolyline :: Integer -> [Point3D] -> [Point3D]
resampleLooped3DPolyline n pts = resamplePolyline interpolateLooped3DPolyline n pts

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
interpolateLooped3DPolyline pts = concatMap (\x -> [interpolate4 x,x !! 2]) $ loopedConsecutives 4 pts

interpolate2DPolyline :: [Point2D] -> [Point2D]
interpolate2DPolyline = map to2d . interpolate3DPolyline . map to3d

repeatInterpolate :: ([a] -> [a]) -> Integer -> [a] -> [a]
repeatInterpolate _ n xs | genericLength xs == n = xs
repeatInterpolate f n xs | genericLength xs < 2*n = repeatInterpolate f n $ f xs
repeatInterpolate _ _ xs = xs

resamplePolyline :: (Lerpable a) => ([a] -> [a]) -> Integer -> [a] -> [a]
resamplePolyline f n pts = 
    let expanded_points = repeatInterpolate f n pts
        polylineFunction = lerpMap $ zip (zeroToOne $ genericLength expanded_points) expanded_points
        in map polylineFunction $ zeroToOne n

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