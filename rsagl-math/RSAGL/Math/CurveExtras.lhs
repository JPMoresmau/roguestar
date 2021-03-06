\section{Specific and Interpolated Curves}

\begin{code}
module RSAGL.Math.CurveExtras
    (sphericalCoordinates,
     cylindricalCoordinates,
     toroidalCoordinates,
     circularCoordinates,
     polarCoordinates,
     transformUnitSquareToUnitCircle,
     transformUnitCubeToUnitSphere,
     circleXY,
     regularPolygon,
     linearInterpolation,
     loopedLinearInterpolation,
     smoothCurve,
     loopCurve)
    where

import RSAGL.Math.Curve
import RSAGL.Math.Interpolation
import RSAGL.Math.Vector
import RSAGL.Math.Angle
import RSAGL.Math.AbstractVector
import RSAGL.Math.Affine
import RSAGL.Math.ListUtils
import Control.Arrow
import RSAGL.Math.Types
\end{code}

\subsection{Alternate Coordinate Systems for Models}

\begin{code}
sphericalCoordinates :: ((Angle,Angle) -> a) -> Surface a
sphericalCoordinates f = transformSurface2 id (clampCurve (0,1)) $ surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,fromRadians $ ((pi/2) - v*pi))))

cylindricalCoordinates :: ((Angle,RSdouble) -> a) -> Surface a
cylindricalCoordinates f = transformSurface2 id (clampCurve (0,1)) $ surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,v)))

toroidalCoordinates :: ((Angle,Angle) -> a) -> Surface a
toroidalCoordinates f = surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,fromRadians $ negate $ v*2*pi)))

circularCoordinates :: ((RSdouble,RSdouble) -> a) -> Surface a
circularCoordinates f = surface $ curry $ (f . second negate . transformUnitSquareToUnitCircle)

polarCoordinates :: ((Angle,RSdouble) -> a) -> Surface a
polarCoordinates f = circularCoordinates (f . cartesianToPolar)
\end{code}

\subsection{Transformations Between Unit Volumes}

\begin{code}
transformUnitSquareToUnitCircle :: (RSdouble,RSdouble) -> (RSdouble,RSdouble)
transformUnitSquareToUnitCircle (u,v) = (x,z)
    where (Point3D x _ z) = transformUnitCubeToUnitSphere (Point3D u 0.5 v)

transformUnitCubeToUnitSphere :: Point3D -> Point3D
transformUnitCubeToUnitSphere p =
    let p_centered@(Point3D x y z) = scale' 2.0 $ translate (Vector3D (-0.5) (-0.5) (-0.5)) p
        p_projected = scale' (minimum [recip $ abs x,recip $ abs y,recip $ abs z]) p_centered
        k = recip $ distanceBetween origin_point_3d p_projected
        w = maximum $ [abs x, abs y, abs z] -- 'w' could be 1, but this gives a smoother tesselation
        in if p_centered == origin_point_3d then origin_point_3d else lerp w (p_centered,scale' k p_centered)
\end{code}

\subsection{Circles}

\begin{code}
circleXY :: Curve Point3D
circleXY = curve $ \u_ -> let u = fromRotations u_ in Point3D (cosine u) (sine u) 0
\end{code}

\subsection{Regular Polygons}

A regular polygon, centered at the origin, in the XY plane.

\begin{code}
regularPolygon :: Integer -> Curve Point3D
regularPolygon n = loopedLinearInterpolation $ map (flip rotateZ (Point3D 0 1 0) . fromRotations) $ zeroToOne n
\end{code}

\subsection{Piecewise Length Normalization}

\begin{code}
normalizePolyline :: (AbstractSubtract p v,AbstractMagnitude v) => [p] -> [(RSdouble,p)]
normalizePolyline pts = zip (map (/ total_dist) accumulated_dists) pts
    where dists = map (uncurry abstractDistance) $ doubles pts
          total_dist = last accumulated_dists
          accumulated_dists = scanl (+) 0 dists
\end{code}

\subsection{Interpolated Curves}

\begin{code}
linearInterpolation :: (AbstractSubtract p v,AbstractAdd p v,AbstractMagnitude v,AbstractScale v) => [p] -> Curve p
linearInterpolation = curve . lerpMap . normalizePolyline

loopedLinearInterpolation :: (AbstractSubtract p v,AbstractAdd p v,AbstractMagnitude v,AbstractScale v) => [p] -> Curve p
loopedLinearInterpolation = loopCurve (0,1) . linearInterpolation . (\a -> last a:a)
\end{code}

\subsection{Smoothing Curves}

\texttt{smoothCurve i h} takes i samples of an h-long piece of a 'Curve' at each point to smooth it.  This is not an interpolation function and will tend to shrink shapes
toward their center of gravity.

\begin{code}
smoothCurve :: (AbstractAdd p v,AbstractSubtract p v,AbstractVector v,AbstractZero p) => Integer -> RSdouble -> Curve p -> Curve p
smoothCurve i h c = curve $ \u -> abstractAverage $ iterateCurve i $ controlCurve (u-h/2,u+h/2) (0,1) c
\end{code}
