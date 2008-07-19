\section{Specific and Interpolated Curves}

\begin{code}
module RSAGL.CurveExtras
    (sphericalCoordinates,
     cylindricalCoordinates,
     toroidalCoordinates,
     planarCoordinates,
     transformUnitSquareToUnitCircle,
     transformUnitCubeToUnitSphere,
     clampV,
     circleXY,
     regularPolygon,
     linearInterpolation,
     loopedLinearInterpolation,
     smoothCurve,
     loopCurve)
    where

import RSAGL.Curve
import RSAGL.Interpolation
import RSAGL.Vector
import RSAGL.Angle
import RSAGL.Auxiliary
import RSAGL.AbstractVector
import Data.Fixed
import RSAGL.Affine
\end{code}

\subsection{Alternate Coordinate Systems for Models}

\begin{code}
sphericalCoordinates :: ((Angle,Angle) -> a) -> Surface a
sphericalCoordinates f = clampV $ surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,fromRadians $ ((pi/2) - v*pi))))

cylindricalCoordinates :: ((Angle,Double) -> a) -> Surface a
cylindricalCoordinates f = clampV $ surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,v)))

toroidalCoordinates :: ((Angle,Angle) -> a) -> Surface a
toroidalCoordinates f = surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,fromRadians $ negate $ v*2*pi)))

planarCoordinates :: Point3D -> Vector3D -> ((Double,Double) -> (Double,Double)) -> Surface (Point3D,Vector3D)
planarCoordinates center upish f = surface (curry $ g . f)
    where (u',v') = orthos upish
          g (u,v) = (translate (vectorScale u u' `vectorAdd` vectorScale v v') center, upish)
\end{code}

\subsection{Transformations Between Unit Volumes}

\begin{code}
transformUnitSquareToUnitCircle :: (Double,Double) -> (Double,Double)
transformUnitSquareToUnitCircle (u,v) = (x,z)
    where (Point3D x _ z) = transformUnitCubeToUnitSphere (Point3D u 0.5 v)

transformUnitCubeToUnitSphere :: Point3D -> Point3D
transformUnitCubeToUnitSphere p =
    let p_centered@(Point3D x y z) = scale' 2.0 $ translate (Vector3D (-0.5) (-0.5) (-0.5)) p
        p_projected = scale' (minimum [recip $ abs x,recip $ abs y,recip $ abs z]) p_centered
        k = recip $ distanceBetween origin_point_3d p_projected
        in if p_centered == origin_point_3d then origin_point_3d else scale' k p_centered
\end{code}

\subsection{Common Pre-Transformations}

\begin{code}
clampV :: Surface a -> Surface a
clampV = pretransformSurface id (min 1 . max 0)
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
normalizePolyline :: (AbstractSubtract p v,AbstractMagnitude v) => [p] -> [(Double,p)]
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
loopedLinearInterpolation = loopCurve . linearInterpolation . (\a -> last a:a)
\end{code}

\subsection{Smoothing Curves}

\texttt{smoothCurve i h} takes i samples of an h-long piece of a curve at each point to smooth it.

\begin{code}
smoothCurve :: (AbstractAdd p v,AbstractSubtract p v,AbstractVector v) => Integer -> Double -> Curve p -> Curve p
smoothCurve i h c = curve (\u -> abstractAverage $ iterateCurve i $ pretransformCurve (flip lerp (u-h/2,u+h/2)) c)
\end{code}

\subsection{Looping Curves}

Curves can be forced to loop with a pre-transformation, however, if the curve does not evaluate to the same value at \texttt{u = 0} and \texttt{u = 1},
this creates a discontinuity in the curve.

\begin{code}
loopCurve :: Curve a -> Curve a
loopCurve = pretransformCurve (`mod'` 1.0)
\end{code}
