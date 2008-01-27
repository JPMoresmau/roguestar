\section{Pre-Specified Animations}

\begin{code}
{-# OPTIONS_GHC -farrows #-}

module RSAGL.AnimationExtras
    (rotationA,
     animateA,
     rotateA,
     pointAtCameraA,
     inverseSquareLaw,
     accelerationModel)
    where

import RSAGL.Vector
import RSAGL.FRP
import RSAGL.Time
import Control.Arrow
import Control.Arrow.Operations
import RSAGL.CoordinateSystems
import RSAGL.Affine
import RSAGL.Angle
import RSAGL.Scene
import RSAGL.Model
import RSAGL.Affine
import RSAGL.WrappedAffine
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
       transformA action -< (at,i)

rotateA :: (Arrow a,ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => Vector3D -> Rate Angle -> FRP i o a j p -> FRP i o a j p
rotateA v a = animateA (rotationA v a)
\end{code}

\subsection{Camera Relative Animators}

\texttt{pointAtCameraA} always points at the camera, using a single rotation.

\begin{code}
pointAtCameraA :: (Arrow a,ArrowState s a,CoordinateSystemClass s,ScenicAccumulator s) => a (SceneLayer,IO IntermediateModel) ()
pointAtCameraA = proc (slayer,imodel) ->
    do cs <- arr getCoordinateSystem <<< fetch -< ()
       accumulateSceneA -< (slayer,cameraRelativeSceneObject $ \c -> 
           liftM ((rotateToFrom (vectorToFrom (migrate root_coordinate_system cs $ if slayer == Infinite then origin_point_3d else camera_position c) $ origin_point_3d)
                                       (Vector3D 0 1 0)) . wrapAffine) imodel)
\end{code}

\subsection{Physical Models}

\texttt{inverse_square_law} represents the general form of the law of gravitation.

\begin{code}
type PV = (Point3D,Rate Vector3D)
type PVA = (Point3D,Rate Vector3D,Acceleration Vector3D)
type ForceFunction = Time -> Point3D -> Rate Vector3D -> Acceleration Vector3D

inverseSquareLaw :: Double -> Point3D -> ForceFunction
inverseSquareLaw g attractor _ p _ = perSecond $ perSecond $ vectorScaleTo (g * (recip $ vectorLengthSquared v)) v
    where v = vectorToFrom attractor p

accelerationModel :: (Arrow a,ArrowChoice a,ArrowApply a,ArrowState s a,CoordinateSystemClass s) => 
                     Frequency -> PV -> FRP i o a j ForceFunction ->
                     FRP i o a (PVA,j) p -> FRP i o a j p
accelerationModel f pv forceA actionA = proc j ->
    do (p,v) <- integralRK4' f (flip translate) pv <<< forceA -< j
       a <- derivative -< v
       transformA actionA -< (translate (vectorToFrom p origin_point_3d),((p,v,a),j))
\end{code}