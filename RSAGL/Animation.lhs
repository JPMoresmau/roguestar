\section{Monads and Arrows for Animation}

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Animation
    (AniM,
     TimePlusSceneAccumulator,
     frameTime,
     runAniM,
     rotationM)
    where

import RSAGL.Time
import RSAGL.Scene
import Control.Monad.State
import RSAGL.CSN
import RSAGL.Angle
import RSAGL.Vector
import RSAGL.Affine
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

rotationM :: Vector3D -> Angle -> AniM (forall a. (AffineTransformable a) => a -> a)
rotationM v a =
    do c <- liftM toSeconds frameTime
       return (rotate v (scaleAngle c a))
\end{code}