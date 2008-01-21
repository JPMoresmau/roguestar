\section{Specific and Interpolated Curves}

\begin{code}
module RSAGL.CurveExtras
    (circleXY,
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