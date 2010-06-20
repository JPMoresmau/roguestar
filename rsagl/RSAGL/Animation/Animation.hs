{-# LANGUAGE Arrows,
             MultiParamTypeClasses,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             TypeFamilies,
             ExistentialQuantification,
             Rank2Types #-}

-- | Supports monadic and arrow operations for animated
-- scenes.
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

-- | A time-aware scene accumulator.
newtype TimePlusSceneAccumulator m = TimePlusSceneAccumulator (Time,SceneAccumulator m)
    deriving (CoordinateSystemClass)

instance (Monad m) => ScenicAccumulator (TimePlusSceneAccumulator m) m where
    accumulateScene slayer scobj (TimePlusSceneAccumulator (t,sceneaccum)) = TimePlusSceneAccumulator (t,accumulateScene slayer scobj sceneaccum)

instance RecombinantState (TimePlusSceneAccumulator m) where
    type SubState (TimePlusSceneAccumulator m) = TimePlusSceneAccumulator m
    clone = id
    recombine (TimePlusSceneAccumulator (t,old)) (TimePlusSceneAccumulator (_,new)) = TimePlusSceneAccumulator (t,recombine old new)

-- | A monad for animation using RSAGL's scene accumulation system.
type AniM a = StateT (TimePlusSceneAccumulator IO) IO a

-- | Get's the time of the current frame.
frameTime :: AniM Time
frameTime = gets (\(TimePlusSceneAccumulator (t,_)) -> t)

runAniM :: AniM (a,SceneLayerInfo) -> IO (a,Scene)
runAniM anim = 
    do t <- getTime
       ((a,sli),TimePlusSceneAccumulator (_,sa)) <- runStateT anim $ TimePlusSceneAccumulator (t,null_scene_accumulator)
       result_scene <- assembleScene sli sa
       return (a,result_scene)

-- | Generates a continuous rotation.
rotationM :: Vector3D -> Rate Angle -> AniM AffineTransformation
rotationM v a =
    do t <- frameTime
       return (rotate v (a `over` t))

-- | Combine an animation with a continuous affine transformation.
animateM :: AniM AffineTransformation -> AniM b -> AniM b
animateM affineF action =
    do at <- affineF
       transformM (affineOf at) action

-- | Rotate an animation continuously.
rotateM :: Vector3D -> Rate Angle -> AniM a -> AniM a
rotateM v a = animateM (rotationM v a)

-- | An object that can capture either a monadic or
-- arrow-based animation.
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

