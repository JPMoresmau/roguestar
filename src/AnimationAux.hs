{-# OPTIONS_GHC -fglasgow-exts #-}

module AnimationAux 
    (newStepAnimation,
     newLerpAnimation,
     newAcceleratedLerpAnimation)
    where
    
import AnimationCore
import Seconds
import Math3D
import Control.Monad
    
type GetA a o = AniM () o (Maybe a)
type AToO a o = (a,o) -> a -> AniM () o o
type RenderO a o = (a,o) -> AniM () o ()
type LerpTime o = o -> o -> AniM () o Seconds
    
stepAnimation :: (Eq a) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> () -> AniM () o o
stepAnimation a o getA atoO renderO _ = 
    do newa <- getA
       case newa of
               Just newa' | a /= newa' -> do newo <- atoO (a,o) newa'
                                             animHardSwitch () $ stepAnimation newa' newo getA atoO renderO
               _ -> do renderO (a,o)
                       return o

-- |
-- A generic step animation.  
-- newStepAnimation a o getA atoO renderO
-- a is a representation of the object to be rendered.  The animation thread watches the value of a returned by getA.
-- If it changes, atoO is run to generate a new value for o, which will be returned until a changes again.
-- atoO recieves the old values of a and o in its first parameter and the value of a in its second parameter.
--
newStepAnimation :: (Eq a) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> IO (Animation () o)
newStepAnimation a o getA atoO renderO = newAnimation $ stepAnimation a o getA atoO renderO

data LerpAnimation o = LerpAnimation { lerpanim_start_seconds :: Seconds,
                                       lerpanim_lerp_seconds :: Seconds,
                                       lerpanim_old :: LerpAnimation o,
                                       lerpanim_new :: o }
                     | LerpAnimationStill o -- since we lerp recursively against old (interrupted) lerps, keep an alternate representation of completed lerps
                                            -- don't lerp recursively against every lerp we've ever done.

-- |
-- Retrieve the most recent un-interpolated value of a LerpAnimation.
--
lerpAnimNewest :: LerpAnimation o -> o
lerpAnimNewest (LerpAnimationStill o) = o
lerpAnimNewest o = lerpanim_new o

-- |
-- Retrieve the second most recent un-interpolated value of a LerpAnimation.
--
{-
lerpAnimLast :: LerpAnimation o -> o
lerpAnimLast (LerpAnimationStill o) = o
lerpAnimLast (LerpAnimation {lerpanim_old = (LerpAnimationStill o)}) = o
lerpAnimLast (LerpAnimation {lerpanim_old = (LerpAnimation {lerpanim_new = o})}) = o

-- |
-- Safely get the lerpanim_start_seconds field of a LerpAnimation.
--
lerpAnimStartSeconds :: LerpAnimation o -> Seconds
lerpAnimStartSeconds (LerpAnimationStill _) = 0
lerpAnimStartSeconds (LerpAnimation {lerpanim_start_seconds=s}) = s

-- |
-- Safely get the lerpanim_lerp_seconds field of a LerpAnimation.
--
lerpAnimLerpSeconds :: LerpAnimation o -> Seconds
lerpAnimLerpSeconds (LerpAnimationStill _) = 0
lerpAnimLerpSeconds (LerpAnimation {lerpanim_lerp_seconds=s}) = s
-}

animLerp :: (Lerpable a Float) => LerpAnimation a -> (Float -> Float) -> AniM i o a
animLerp (LerpAnimationStill x) _ = return x
animLerp x lerpMutator = 
    do oldx <- animLerp (lerpanim_old x) lerpMutator
       secs <- animSeconds
       return $ lerp (lerpMutator $ fromRational $ max 0 $ min 1 $ (secs - lerpanim_start_seconds x) / (max 0.1 $ lerpanim_lerp_seconds x)  :: Float) (oldx,lerpanim_new x)

optimizeLerp :: LerpAnimation a -> AniM i o (LerpAnimation a)
optimizeLerp x@(LerpAnimationStill _) = return x
optimizeLerp x@(LerpAnimation {}) = 
    do lerp_is_done <- liftM (lerpanim_start_seconds x + lerpanim_lerp_seconds x <) animSeconds
       if lerp_is_done
           then return $ LerpAnimationStill $ lerpanim_new x
           else liftM (\old -> x { lerpanim_old = old }) $ optimizeLerp $ lerpanim_old x

lerpAnimation :: (Eq a,Lerpable o Float) => a -> LerpAnimation o -> GetA a o -> AToO a o -> RenderO a o -> LerpTime o -> (Float -> Float) -> () -> AniM () o o
lerpAnimation a o getA atoO renderO lerpTimeO lerpMutator _ = 
    do newa <- getA
       secs <- animSeconds
       case newa of
               Just newa' | a /= newa' -> do newo <- atoO (a,lerpAnimNewest o) newa'
                                             lerp_time <- lerpTimeO (lerpAnimNewest o) newo
                                             o' <- optimizeLerp o
                                             let newo' = LerpAnimation { lerpanim_start_seconds = secs,
                                                                         lerpanim_lerp_seconds = lerp_time,
                                                                         lerpanim_old = o',
                                                                         lerpanim_new = newo }
                                             animHardSwitch () $ lerpAnimation newa' newo' getA atoO renderO lerpTimeO lerpMutator
               _ -> do lerped_o <- animLerp o lerpMutator
                       renderO (a,lerped_o)
                       return lerped_o

-- |
-- A generic linear-interpolated animation.  This works the same as newStepAnimation, with a new function,
-- lerpTime, which indicates how long the linear interpolation should take.  For example, this might be the
-- pythagorean distance between two points, or just a constant value.
--
newLerpAnimation :: (Eq a,Lerpable o Float) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> LerpTime o -> IO (Animation () o)
newLerpAnimation a o getA atoO renderO lerpTime = newAnimation $ lerpAnimation a (LerpAnimationStill o) getA atoO renderO lerpTime id

-- |
-- Another linear-interpolated animation.  This time, the interpolation is non-linear wrt time.  Intead, the animated
-- object accelerates to a maximum speed halfway through the interpolation, and the slows as it approaches the end.
-- No change should be needed to replace newLerpAnimation with newAcceleratedLerpAnimation.
--
newAcceleratedLerpAnimation :: (Eq a,Lerpable o Float) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> LerpTime o -> IO (Animation () o)
newAcceleratedLerpAnimation a o getA atoO renderO lerpTime = newAnimation $ lerpAnimation a (LerpAnimationStill o) getA atoO renderO (accelerateTime lerpTime) accelerateLerp
    where accelerateLerp x | x < 0.5 = 0.5 * (2 * x) ^ 2
          accelerateLerp x = 1.0 - 0.5 * (2 * (1-x)) ^ 2
          accelerateTime fn x y = (return . (toRational . (* 2) . sqrt . fromRational)) =<< fn x y
