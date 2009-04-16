\section{Extrusions}

\begin{code}
module RSAGL.Modeling.Extrusion
    (extrude,
     extrudeTube,
     extrudePrism)
    where

import RSAGL.Math.Curve
import RSAGL.Math.CurveExtras
import RSAGL.Math.Vector
import RSAGL.Math.Affine
import Control.Applicative
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Orthagonal
import Data.Maybe
\end{code}

\subsection{The General Extrusion}

\texttt{extrude} takes a vector or point indicating an orientation, a curve representing the spine of the extrusion, and
a curve representing the loop in the x-y plane.

\begin{code}
extrude :: Curve (Either Point3D Vector3D) -> Curve Point3D -> Curve (Curve Point3D) -> Surface Point3D
extrude upish spine loop = wrapSurface $ transformation <$> (modelLookAt <$> spine <*> (forward <$> Right <$> spine') <*> (up <$> upish)) <*> loop
    where spine' = curveDerivative spine
\end{code}

\subsection{Specific Extrusions}

\begin{code}
extrudeTube :: Curve Double -> Curve Point3D -> Surface Point3D
extrudeTube radius spine = extrude upish spine (scale' <$> radius <*> pure circleXY)
    where upish = pure $ Right $ fromMaybe (let [a,b] = iterateCurve 2 spine in fst $ orthos $ vectorToFrom a b) $ newellCurve spine

extrudePrism :: Vector3D -> (Point3D,Double) -> (Point3D,Double) -> Curve Point3D -> Surface Point3D
extrudePrism upish (a,ra) (b,rb) c = extrude (pure $ Right $ upish) (linearInterpolation [a,b]) (flip scale' c <$> linearInterpolation [ra,rb])
\end{code}
