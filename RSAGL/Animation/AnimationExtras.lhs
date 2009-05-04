\section{Specific Animations}

\begin{code}
{-# LANGUAGE Arrows #-}

module RSAGL.Animation.AnimationExtras
    (rotationA,
     animateA,
     rotateA,
     pointAtCameraA,
     inverseSquareLaw,
     quadraticTrap,
     drag,
     concatForces,
     constrainForce,
     accelerationModel)
    where

import RSAGL.Math.Vector
import RSAGL.FRP.FRP
import RSAGL.FRP.Time
import RSAGL.Math.AbstractVector
import Control.Arrow
import Control.Arrow.Operations
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Affine
import RSAGL.Math.Angle
import RSAGL.Scene.Scene
import RSAGL.Modeling.Model
import RSAGL.Math.Affine
import RSAGL.Math.WrappedAffine
import Control.Monad
\end{code}

\subsection{Simple Animators}

\begin{code}
rotationA :: (Arrow a,ArrowChoice a) => Vector3D -> Rate Angle -> FRP i o a ignored AffineTransformation
rotationA v a = proc _ ->
    do t <- absoluteTime -< ()
       returnA -< rotate v (a `over` t)

animateA :: (Arrow a,ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => FRP i o a j AffineTransformation -> FRP i o a j p -> FRP i o a j p
animateA affineA action = proc i ->
    do at <- affineA -< i
       transformA action -< (affineOf at,i)

rotateA :: (Arrow a,ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => Vector3D -> Rate Angle -> FRP i o a j p -> FRP i o a j p
rotateA v a = animateA (rotationA v a)
\end{code}

\subsection{Camera Relative Animators}

\texttt{pointAtCameraA} always points at the camera, using a single rotation.

\begin{code}
pointAtCameraA :: (Arrow a,ArrowState s a,CoordinateSystemClass s,ScenicAccumulator s m,ModelType model) => a (SceneLayer,m model) ()
pointAtCameraA = proc (slayer,the_model) ->
    do cs <- arr getCoordinateSystem <<< fetch -< ()
       accumulateSceneA -< (slayer,cameraRelativeSceneObject $ \c -> 
           liftM ((rotateToFrom (vectorToFrom (migrateToFrom root_coordinate_system cs $ camera_position c) $ origin_point_3d)
                                       (Vector3D 0 1 0)) . wrapAffine) (liftM toIntermediateModel the_model))
\end{code}

\subsection{Particle Physics Models}

\begin{code}
type PV = (Point3D,Rate Vector3D)
type PVA = (Point3D,Rate Vector3D,Acceleration Vector3D)
type ForceFunction = Time -> Point3D -> Rate Vector3D -> Acceleration Vector3D
\end{code}

\texttt{inverseSquareLaw} produces a \texttt{ForceFunction} that attracts a particle as though by
a gravitational singularity.

\begin{code}
inverseSquareLaw :: Double -> Point3D -> ForceFunction
inverseSquareLaw g attractor _ p _ = perSecond $ perSecond $ vectorScaleTo (g * (recip $ vectorLengthSquared v)) v
    where v = vectorToFrom attractor p
\end{code}

\texttt{quadraticTrap} is the inverse of the inverse square law.  Because the attraction increases with
distance, all particles are trapped (there is no escape velocity).

\begin{code}
quadraticTrap :: Double -> Point3D -> ForceFunction
quadraticTrap g attractor _ p _ = perSecond $ perSecond $ vectorScaleTo (g * vectorLengthSquared v) v
    where v = vectorToFrom attractor p
\end{code}

\texttt{drag} behaves like simple aerodynamic drag.

\begin{code}
drag :: Double -> ForceFunction
drag x _ _ v' = perSecond $ perSecond $ vectorScaleTo (negate $ x * vectorLengthSquared v) v
    where v = v' `over` fromSeconds 1
\end{code}

\texttt{concatForces} combines any arbitrary group of \texttt{ForceFunction}s.

\begin{code}
concatForces :: [ForceFunction] -> ForceFunction
concatForces ffs t p v = abstractSum $ map (\f -> f t p v) ffs
\end{code}

\texttt{constrainForce} acts as a conditional for \texttt{ForceFunction}s.

\begin{code}
constrainForce :: (Time -> Point3D -> Rate Vector3D -> Bool) -> ForceFunction -> ForceFunction
constrainForce predicate f t p v = if predicate t p v
    then f t p v
    else zero
\end{code}

\texttt{accelerationModel} implements the \texttt{ForceFunction}s on a single particle.

\begin{code}
accelerationModel :: (Arrow a,ArrowChoice a,ArrowApply a,ArrowState s a,CoordinateSystemClass s) => 
                     Frequency -> PV -> FRPX any t i o a j ForceFunction ->
                     FRPX any t i o a (PVA,j) p -> FRPX any t i o a j p
accelerationModel f pv forceA actionA = proc j ->
    do (p,v) <- integralRK4' f (flip translate) pv <<< forceA -< j
       a <- derivative -< v
       transformA actionA -< (affineOf $ translate (vectorToFrom p origin_point_3d),((p,v,a),j))
\end{code}
