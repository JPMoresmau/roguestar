{-# OPTIONS_GHC -fglasgow-exts #-}

module AnimationAux 
    (instanceEdgeDetector,
     instancePersistantAnimation,
     instanceStatefulAnimation,
     instanceHistoryTracker,
     animStatefulHardSwitch,
     animStatefulSoftSwitch,
     instanceLerpAnimation,
     instanceLerpAnimationMutated,
     animGetAnswer,
     animGetTable,
     DataFreshness(..))
    where
    
import AnimationCore
import Globals
import Time
import Math3D
import Control.Monad
import Control.Monad.State
import Data.IORef
import Driver
import Tables
import Data.Maybe
    
{-
type GetA a o = AniM () o (Maybe a)
type AToO a o = (a,o) -> a -> AniM () o o
type RenderO a o = (a,o) -> AniM () o ()
type LerpTime o = o -> o -> AniM () o Time
-}

type AniMState s i o a = StateT s (AniM i o) a

-- |
-- Creates a stateful animation.
--
instanceStatefulAnimation :: (AnimationSource m) => r -> s -> (a -> AniMState s a r r) -> m (a -> AniM i o r)
instanceStatefulAnimation default_value initial_state fn = instanceAnimation default_value (statefulAnimation fn initial_state)

statefulAnimation :: (i -> AniMState s i o o) -> s -> i -> AniM i o o
statefulAnimation fn state x = do (o,s) <- runStateT (fn x) state
                                  animTerminate (statefulAnimation fn s) o

animStatefulHardSwitch :: i -> (i -> AniMState s i o o) -> AniMState s i o o
animStatefulHardSwitch param fn = do s <- get
                                     lift $ animHardSwitch param (statefulAnimation fn s) 

animStatefulSoftSwitch :: (i -> AniMState s i o o) -> AniMState s i o ()
animStatefulSoftSwitch fn = do state <- get
                               lift $ animSoftSwitch $ statefulAnimation fn state

instanceEdgeDetector :: (Eq a,AnimationSource m) => a -> m (a -> AniM i o ((a,a),Time))
instanceEdgeDetector initial_value =
    do instanceStatefulAnimation (error "edgeDetectionAnimation: failed") ((initial_value,initial_value),fromSeconds 0) edgeDetector
           where edgeDetector :: (Eq a) => a -> AniMState ((a,a),Time) a ((a,a),Time) ((a,a),Time)
                 edgeDetector x = do ((_,nowx),_) <- get
                                     now <- lift animTime
                                     when (nowx /= x) $ put ((nowx,x),now)
                                     get

-- |
-- Wraps a function that may fail.  If the function fails,
-- the animation returns the most recent value of that function.
--
instancePersistantAnimation :: (AnimationSource m) => r -> (a -> AniM a (Maybe r) (Maybe r)) -> m (a -> AniM i o r)
instancePersistantAnimation initial_value unwrapped_fn =
    do fn <- instanceAnimation (error "persistantAnimation: wrapped animation failed") unwrapped_fn
       let persistantAnimation param =
               do maybe_value <- lift $ fn param
                  when (isJust maybe_value) $ put $ fromJust maybe_value
                  get
       instanceStatefulAnimation (error "persistantAnimation: failed") initial_value persistantAnimation

-- |
-- Answers an edge-detection history for a value.
-- A function, given the current time, indicates when old values should be kept (True) or discarded (False).
-- The history tracker is guaranteed never to return an empty list.
--
instanceHistoryTracker :: (Eq a,AnimationSource m) => a -> (Time -> ((a,a),Time) -> Bool) -> m (a -> AniM i o [((a,a),Time)])
instanceHistoryTracker initial_value forgetfn = 
    do edgeDetector <- instanceEdgeDetector initial_value
       let historyTracker param = 
               do value <- lift $ edgeDetector param
                  time <- lift $ animTime
                  history <- gets (takeWhile $ forgetfn time)
                  when (Just value /= listToMaybe history) $ modify (const $ value : history)
                  get
       instanceStatefulAnimation (error "historyTracker: failed") [((initial_value,initial_value),fromSeconds 0)] historyTracker

-- |
-- Instantiate a new animation to be called from another.
-- The first parameter is a default value to return if the animation ever fails.
--
instanceAnimation :: (AnimationSource m) => r -> (a -> AniM a r r) -> m (a -> AniM i o r)
instanceAnimation default_value anim_fn =
    do animation <- newAnimation anim_fn
       return $ liftM (fromMaybe default_value) . animCall animation

-- |
-- Use a duration function to smoothly lerp a changing value.
-- The duration function returns the time needed to lerp between two values.
--
instanceLerpAnimation :: (AnimationSource m,Eq a,Lerpable a) => a -> (a -> a -> Time) -> m (a -> AniM i o a)
instanceLerpAnimation = instanceLerpAnimationMutated id

instanceLerpAnimationMutated :: (AnimationSource m,Eq a,Lerpable a) => (Double->Double) -> a -> (a -> a -> Time) -> m (a -> AniM i o a)
instanceLerpAnimationMutated mutator initial_value duration =
    do historyTracker <- instanceHistoryTracker initial_value (\t_now ((x,y),t_then) -> t_now <= t_then + duration x y)
       let lerpfn1 ((a,a'),_) | duration a a' == 0 =
               do return a'
           lerpfn1 ((a,a'),at) =
               do time <- animTime
                  return $ lerpBetweenClampedMutated mutator (at,time,at + duration a a') (a,a')
           lerpfn2 aevent ((_,b'),bt) =
               do b <- lerpfn1 aevent
                  return ((b,b'),bt)
       let lerpAnimation param =
               do history <- liftM reverse $ historyTracker param
                  lerpfn1 =<< foldM lerpfn2 (head history) (tail history)
       instanceAnimation (error "lerpAnimation: failed") lerpAnimation

-- |
-- driverGetAnswer, embedded in the Animation Monad.
--
animGetAnswer :: IORef RoguestarGlobals -> DataFreshness -> String -> AniM i o (Maybe String)
animGetAnswer globals_ref freshness s = AnimationCore.unsafeAnimIO $ driverGetAnswer globals_ref freshness s

-- |
-- driverGetTable, embedded in the Animation Monad.
--
animGetTable :: IORef RoguestarGlobals -> DataFreshness -> String -> String -> AniM i o (Maybe RoguestarTable)
animGetTable globals_ref freshness the_table_name the_table_id = 
        AnimationCore.unsafeAnimIO $ driverGetTable globals_ref freshness the_table_name the_table_id