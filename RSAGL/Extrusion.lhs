\section{Extrusions}

\begin{code}
module RSAGL.Extrusion
    (extrude,
     extrudeTube,
     extrudePrism,
     extrudeSmoothRegularPrism)
    where

import RSAGL.Curve
import RSAGL.CurveExtras
import RSAGL.Vector
import RSAGL.Affine
import Control.Applicative
\end{code}

\subsection{The General Extrusion}

\texttt{extrude} takes a vector or point indicating an orientation, a curve representing the spine of the extrusion, and
a curve representing the loop in the x-y plane.

\begin{code}
extrude :: Curve (Either Point3D Vector3D) -> Curve Point3D -> Curve (Curve Point3D) -> Surface Point3D
extrude upish spine loop = wrapSurface $ transformation <$> (modelLookAt <$> spine <*> (forward <$> Right <$> spine') <*> (up <$> upish)) <*> loop
    where spine' = curveDerivative3D spine
\end{code}

\subsection{Specific Extrusions}

\begin{code}
extrudeTube :: Curve Double -> Curve Point3D -> Surface Point3D
extrudeTube radius spine = extrude upish spine (scale' <$> radius <*> pure circleXY)
    where upish = pure $ Right $  newell $ iterateCurve 25 spine

extrudePrism :: Vector3D -> (Point3D,Double) -> (Point3D,Double) -> Curve Point3D -> Surface Point3D
extrudePrism upish (a,ra) (b,rb) c = extrude (pure $ Right $ upish) (linearInterpolation [a,b]) (flip scale' c <$> linearInterpolation [ra,rb])

extrudeSmoothRegularPrism :: (Point3D,Double) -> (Point3D,Double) -> Integer -> Surface Point3D
extrudeSmoothRegularPrism ara brb n = extrudePrism (fst $ orthos $ vectorToFrom (fst brb) (fst ara)) ara brb (smoothCurve 3 (recip $ realToFrac n * 2) $ regularPolygon n)
\end{code}
