\section{Monads and Arrows for Animation}

The \texttt{AniM} monad and the \texttt{AniA} arrow support frame time, affine transformation and scene accumulation.

\begin{code}
{-# LANGUAGE Arrows,
             MultiParamTypeClasses,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             TypeFamilies,
             ExistentialQuantification,
             Rank2Types #-}

module RSAGL.Animation.Animation
    (AniM,
     TimePlusSceneAccumulator,
     frameTime,
     runAniM,
     rotationM,
     animateM,
     rotateM,
     AnimationObject,
     newAnimationObjectM,
     newAnimationObjectA,
     runAnimationObject)
    where

import RSAGL.Scene.Scene
import Control.Monad.State
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Angle
import RSAGL.Math.Vector
import RSAGL.Math.Affine
import RSAGL.FRP
import RSAGL.Auxiliary.RecombinantState
\end{code}

\subsection{The AniM Monad}

The AniM monad is a simple state layer over the IO monad that supports scene accumulation and getting the frame time (the time at the beginning of the frame, as opposed to real time that would change as the animation progresses).

\begin{code}
newtype TimePlusSceneAccumulator m = TimePlusSceneAccumulator (Time,SceneAccumulator m)
    deriving (CoordinateSystemClass)

instance (Monad m) => ScenicAccumulator (TimePlusSceneAccumulator m) m where
    accumulateScene slayer scobj (TimePlusSceneAccumulator (t,sceneaccum)) = TimePlusSceneAccumulator (t,accumulateScene slayer scobj sceneaccum)

instance RecombinantState (TimePlusSceneAccumulator m) where
    type SubState (TimePlusSceneAccumulator m) = TimePlusSceneAccumulator m
    clone = id
    recombine (TimePlusSceneAccumulator (t,old)) (TimePlusSceneAccumulator (_,new)) = TimePlusSceneAccumulator (t,recombine old new)

type AniM a = StateT (TimePlusSceneAccumulator IO) IO a

frameTime :: AniM Time
frameTime = gets (\(TimePlusSceneAccumulator (t,_)) -> t)

runAniM :: AniM (a,SceneLayerInfo) -> IO (a,Scene)
runAniM anim = 
    do t <- getTime
       ((a,sli),TimePlusSceneAccumulator (_,sa)) <- runStateT anim $ TimePlusSceneAccumulator (t,null_scene_accumulator)
       result_scene <- assembleScene sli sa
       return (a,result_scene)

rotationM :: Vector3D -> Rate Angle -> AniM AffineTransformation
rotationM v a =
    do t <- frameTime
       return (rotate v (a `over` t))

animateM :: AniM AffineTransformation -> AniM b -> AniM b
animateM affineF action =
    do at <- affineF
       transformM (affineOf at) action

rotateM :: Vector3D -> Rate Angle -> AniM a -> AniM a
rotateM v a = animateM (rotationM v a)
\end{code}

\subsection{Animation Objects}

This is one possible implementation of an animation object.

\begin{code}
data AnimationObject i o =
    AniMObject (i -> AniM o)
  | AniAObject (FRPProgram (SceneAccumulator IO) i o)

newAnimationObjectM :: (i -> AniM o) -> AnimationObject i o
newAnimationObjectM = AniMObject

newAnimationObjectA :: (forall e. FRP e (FRP1 (SceneAccumulator IO) i o) i o) -> IO (AnimationObject i o)
newAnimationObjectA thread = liftM AniAObject $ newFRP1Program thread

runAnimationObject :: AnimationObject i o -> i -> AniM o
runAnimationObject (AniMObject f) i = f i
runAnimationObject (AniAObject frpp) i =
    do TimePlusSceneAccumulator (t,old_s) <- get
       (o,new_s) <- liftIO $ updateFRPProgram (Just t) (i,old_s) frpp
       put $ TimePlusSceneAccumulator (t,new_s)
       return o
\end{code}
