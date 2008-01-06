\section{Monads and Arrows for Animation}

The \texttt{AniM} monad and the \texttt{AniA} arrow support frame time, affine transformation and scene accumulation.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts -farrows #-}

module RSAGL.Animation
    (AniM,
     TimePlusSceneAccumulator,
     frameTime,
     runAniM,
     rotationM,
     animateM,
     rotateM,
     AniA,
     rotationA,
     animateA,
     rotateA,
     AnimationObject,
     newAnimationObjectM,
     newAnimationObjectA,
     runAnimationObject)
    where

import Data.Monoid
import RSAGL.Time
import RSAGL.Scene
import Control.Monad.State
import RSAGL.CSN
import RSAGL.Angle
import RSAGL.Vector
import RSAGL.Affine
import RSAGL.FRP
import Control.Concurrent.MVar
import Control.Arrow.Transformer.State as StateArrow
import Control.Arrow
\end{code}

\subsection{The AniM Monad}

The AniM monad is a simple state layer over the IO monad that supports scene accumulation and getting the frame time (the time at the beginning of the frame, as opposed to real time that would change as the animation progresses).

\begin{code}
newtype TimePlusSceneAccumulator = TimePlusSceneAccumulator (Time,SceneAccumulator)
    deriving (CoordinateSystemClass, ScenicAccumulator)

type AniM a = StateT TimePlusSceneAccumulator IO a

frameTime :: AniM Time
frameTime = gets (\(TimePlusSceneAccumulator (t,_)) -> t)

runAniM :: AniM (a,Camera) -> IO (a,Scene)
runAniM anim = 
    do time <- getTime
       ((a,c),TimePlusSceneAccumulator (_,sa)) <- runStateT anim $ TimePlusSceneAccumulator (time,null_scene_accumulator)
       return (a,assembleScene c sa)

rotationM :: Vector3D -> Rate Angle -> AniM AffineTransformation
rotationM v a =
    do t <- frameTime
       return (rotate v (a `over` t))

animateM :: AniM AffineTransformation -> AniM b -> AniM b
animateM affineF action =
    do at <- affineF
       transformM at action

rotateM :: Vector3D -> Rate Angle -> AniM a -> AniM a
rotateM v a = animateM (rotationM v a)
\end{code}

\subsection{The AniA Arrow}

\begin{code}
type AniA i o j p = FRP i o (StateArrow SceneAccumulator (->)) j p

rotationA :: (Arrow a,ArrowChoice a) => Vector3D -> Rate Angle -> FRP i o a () AffineTransformation
rotationA v a = proc () ->
    do t <- absoluteTime -< ()
       returnA -< rotate v (a `over` t)

animateA :: AniA i o () AffineTransformation -> AniA i o j p -> AniA i o j p
animateA affineA action = proc i ->
    do at <- affineA -< ()
       transformA action -< (at,i)

rotateA :: Vector3D -> Rate Angle -> AniA i o j p -> AniA i o j p
rotateA v a = animateA (rotationA v a)
\end{code}

\subsection{Animation Objects}

\begin{code}
data AnimationObject i o =
    AniMObject (i -> AniM o)
  | AniAObject (MVar (FRPProgram (StateArrow SceneAccumulator (->)) i o))

newAnimationObjectM :: (i -> AniM o) -> AnimationObject i o
newAnimationObjectM = AniMObject

newAnimationObjectA :: (Monoid o) => [AniA i o i o] -> IO (AnimationObject i o)
newAnimationObjectA frp_threads = liftM AniAObject $ newMVar $ newFRPProgram frp_threads

runAnimationObject :: AnimationObject i o -> i -> AniM o
runAnimationObject (AniMObject f) i = f i
runAnimationObject (AniAObject mv) i =
    do old_frpp <- liftIO $ takeMVar mv
       TimePlusSceneAccumulator (t,old_s) <- get
       let ((o,new_frpp),new_s) = (StateArrow.runState $ updateFRPProgram old_frpp) ((i,t),old_s)
       put $ TimePlusSceneAccumulator (t,new_s)
       liftIO $ putMVar mv new_frpp
       return o
\end{code}