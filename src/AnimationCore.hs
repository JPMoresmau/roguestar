{-# OPTIONS_GHC -fglasgow-exts #-}

module AnimationCore
    (Animation,
     AnimationSource(..),
     ioNewAnimation,
     runAnimationWithContext,
     AniM,
     animSoftSwitch,
     animHardSwitch,
     animReturn,
     animTerminate,
     animToCSN,
     animFromCSN,
     animTime,
     animDeltaTime,
     animGetCoordinateSystem,
     animCall,
     animExcludeIO,
     toCSN,
     fromCSN,
     listCSN,
     unlistCSN,
     pairCSN,
     unpairCSN,
     modifyCSN,
     animModifyCSN,
     CSN,
     CoordinateSystem,
     world_coordinates,
     animio,
     unsafeAnimIO)
    where

import Time
import Math3D
import Model
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe
import Data.List
import Data.IORef
import Tables
import System.IO

type Animation i o = IORef (AnimationData i o)

data AnimationData i o = AnimationData { animdata_last_out :: Maybe (o,[AnimatedIOAction]),
                                         animdata_last_update :: Time,
                                         animdata_switch :: i -> AniM i o o,
                                         animdata_in_progress :: Bool,
                                         animdata_pending_events :: [i]  -- in reverse order of arrival
                                             }

class (Monad m) => AnimationSource m where
    newAnimation :: (i -> AniM i o o) -> m (Animation i o)
    
instance AnimationSource IO where
    newAnimation = ioNewAnimation
    
instance AnimationSource (AniM i o) where
    newAnimation = unsafeAnimIO . ioNewAnimation

ioNewAnimation :: (i -> AniM i o o) -> IO (Animation i o)
ioNewAnimation switch = newIORef $ AnimationData { animdata_last_out = Nothing,
                                                 animdata_last_update = 0.0,
                                                 animdata_switch = switch,
                                                 animdata_in_progress = False,
                                                 animdata_pending_events = [] }

runAnimationWithContext :: CSN Point3D -> Animation i o -> i -> IO (Maybe o)
runAnimationWithContext camerapt animation i = 
    do time <- getTime
       result <- guardedRunAnimation time animation i
       case result of
                   Just (o,ioactions) -> do runAnimationIOActions camerapt ioactions
                                            return $ Just o
                   _ -> do return Nothing

-- |
-- Animations are not re-entrant.  Notify another (or this) animation of an event.
-- The event will be processed lazily.
--
notifyAnimation :: Animation a b -> a -> IO ()
notifyAnimation animation event = modifyIORef animation (\x -> x { animdata_pending_events = event : animdata_pending_events x })

guardedRunAnimation :: Time -> Animation i o -> i -> IO (Maybe (o,[AnimatedIOAction]))
guardedRunAnimation secs animation i =
    do animation_data <- readIORef animation
       case (animdata_in_progress animation_data,
             animdata_last_update animation_data >= secs && null (animdata_pending_events animation_data),
             animdata_last_out animation_data) of
                                   (True,_,result) -> do notifyAnimation animation i
                                                         return result
                                   (False,True,Just result) -> return $ Just result        -- use a cached result
                                   _ -> do runPendingEvents secs animation
                                           unguardedRunAnimation secs animation i  -- run the animation

unguardedRunAnimation :: Time -> Animation i o -> i -> IO (Maybe (o,[AnimatedIOAction]))
unguardedRunAnimation secs animation i =
    do modifyIORef animation (\x -> x { animdata_in_progress = True })
       animdata <- readIORef animation
       let (AniM switch) = animdata_switch animdata i
       (e,s) <- runStateT (runErrorT switch) $ (animationStartState secs animdata)
       result <- case e of
              Left (AniMFail {anim_terminal_msg=msg}) -> 
                      do when (msg /= "") $ hPutStrLn stderr msg
                         forceSwitch animation (\_ -> fail "")
                         return Nothing
              Left a@(AniMTerminal {anim_terminal_msg=msg}) -> 
                      do when (msg /= "") $ hPutStrLn stderr msg
                         forceSwitch animation (anim_terminal_switch a)
                         return $ Just $ (anim_terminal_retval a,anim_terminal_io_actions a)
              Right o -> do forceSwitch animation (anim_switch s)
                            return $ Just (o,anim_io_actions s)
       modifyIORef animation (\x -> x { animdata_in_progress = False,
                                        animdata_last_update = secs,
                                        animdata_last_out = result `mplus` animdata_last_out x })
       return result

runPendingEvents :: Time -> Animation i o -> IO ()
runPendingEvents time animation =
    do pending_events <- liftM (reverse . animdata_pending_events) $ readIORef animation
       modifyIORef animation (\x -> x { animdata_pending_events = [] })
       mapM_ (unguardedRunAnimation time animation) pending_events

forceSwitch :: Animation i o -> (i -> AniM i o o) -> IO ()
forceSwitch animation switch = modifyIORef animation (\x -> x { animdata_switch = switch })

-- |
-- Call another Animation from within an Animation.  All Animations operate within the
-- same world_coordinates: affine-transforming an animCall has no effect.
-- Furthermore, Animations are non-reentrant because they are instanced and stateful.
-- The Animation system will automatically detect mutually-recursive or self-referencing animCalls and
-- provide the most recent result of an animation that is currently in progress.
-- In defense of this design, any other system with such a mutual dependency would need to retain 
-- one-step-old data anyway.
--
animCall :: Animation a b -> a -> AniM i o (Maybe b)
animCall animation a = 
    do time <- animTime
       result <- AniM $ liftIO $ guardedRunAnimation time animation a
       case result of
                   Just (o,ioactions) -> do mapM_ (\x -> animCSNio (toCSN world_coordinates $ animio_position x) (toCSN world_coordinates $ animio_action x)) ioactions
                                            return $ Just o
                   Nothing -> return Nothing

-- |
-- Run an AniM action without accumulating any IO actions that might be performed therein.
--
animExcludeIO :: AniM i o a -> AniM i o a
animExcludeIO anim = 
    do io_actions <- AniM $ gets anim_io_actions
       result <- anim
       AniM $ modify (\x -> x { anim_io_actions = io_actions })
       return result

runAnimationIOActions :: CSN Point3D -> [AnimatedIOAction] -> IO ()
runAnimationIOActions csncamera actions = 
        foldr (>>) (return ()) $ map animio_action $ sortBy sortfn actions
                       where sortfn x y = compare (distanceBetween camera (animio_position x)) (distanceBetween camera (animio_position y))  
                             camera = fromCSN world_coordinates csncamera

data AniMState i o = AniMState { anim_transformation_stack :: [Matrix Double],
                                 anim_io_actions :: [AnimatedIOAction],
                                 anim_switch :: i -> AniM i o o,
                                 anim_time :: Time,
                                 anim_delta_time :: Time }

animationStartState :: Time -> AnimationData i o -> AniMState i o
animationStartState time animdata = 
    AniMState { anim_transformation_stack = [identityMatrix 4],
                anim_io_actions = [],
                anim_switch = animdata_switch animdata,
                anim_time = time,
                anim_delta_time = time - animdata_last_update animdata }

data AniMTerminal i o = AniMTerminal { anim_terminal_retval :: o,
                                       anim_terminal_switch :: i -> AniM i o o,
                                       anim_terminal_io_actions :: [AnimatedIOAction],
                                       anim_terminal_msg :: String }
                      | AniMFail { anim_terminal_msg :: String }

data AnimatedIOAction = AnimatedIOAction { animio_position :: Point3D,
                                           animio_action :: IO () }

-- |
-- The Animation Monad provides a number of services to aid in the description of 
-- realtime OpenGL animations.
--
-- The services of the Animation Monad include:
--
-- Retrive data from the engine (as driverGetTable-Answer).
-- Record OpenGL IO actions with metainformation for later, out-of-order playback.
-- Support sorting of geometry to get correct blending results.
-- Manage arbitrarily large numbers of light sources.
-- Describe light sources alongside models (i.e. a light source inside a model of a lamp post).
-- Store and retrieve spatial information in coordinate system neutral (CSN) form.
-- Respond to events with persistant state transitions (switches).
--
data AniM i o a = AniM (ErrorT (AniMTerminal i o) (StateT (AniMState i o) IO) a)

instance Error (AniMTerminal i o) where
    noMsg = strMsg "AniMTerminal: unspecified animation error without result"
    strMsg str = AniMFail { anim_terminal_msg = str }

instance Monad (AniM i o) where
  (AniM m) >>= k = AniM $ m >>= (\x -> let (AniM k') = k x
                                           in k')
  return a = AniM $ return a
  fail str = AniM $ fail str

-- |
-- Switch but continue the current thread of execution.
--
animSoftSwitch :: (i -> AniM i o o) -> AniM i o ()
animSoftSwitch newswitch = AniM $ modify (\x -> x { anim_switch = newswitch })

-- |
-- Switch and shift execution to the switched thread.
--
animHardSwitch :: i -> (i -> AniM i o o) -> AniM i o o
animHardSwitch i newswitch =
    do animSoftSwitch newswitch
       newswitch i >>= animReturn

-- |
-- End execution with the specified return value; similar to return in an imperative language.
--
animReturn :: o -> AniM i o o
animReturn retval = 
    do switch <- AniM $ gets anim_switch
       io_actions <- AniM $ gets anim_io_actions
       AniM $ throwError $ AniMTerminal { anim_terminal_retval = retval,
                                          anim_terminal_switch = switch,
                                          anim_terminal_io_actions = io_actions,
                                          anim_terminal_msg = "" }
       error "animReturn: unreachable statement"

-- |
-- End execution with a switch and return value.
--
animTerminate :: (i -> AniM i o o) -> o -> AniM i o o
animTerminate switch retval =
    do animSoftSwitch switch
       animReturn retval

animPush :: Matrix Double -> AniM i o ()
animPush mat = AniM $ modify (\x -> x { anim_transformation_stack = mat `matrixMultiply` (head $ anim_transformation_stack x) : anim_transformation_stack x })

animPop :: AniM i o ()
animPop = AniM $ modify (\x -> x { anim_transformation_stack = tail $ anim_transformation_stack x })

animMatrix :: AniM i o (Matrix Double)
animMatrix = AniM $ gets (fromMaybe (identityMatrix 4) . listToMaybe . anim_transformation_stack)

instance AffineTransformable (AniM i o a) where
    transform mat anim = do animPush mat
                            result <- anim
                            animPop
                            return result

animTransform :: AffineTransformable a => a -> AniM i o a
animTransform a = liftM (`transform` a) animMatrix

animInverseTransform :: AffineTransformable a => a -> AniM i o a
animInverseTransform a = liftM ((`transform` a) . matrixInverse) animMatrix

-- |
-- An opaque type to store spatial data in a Coordinate Systen Neutral way.
--
newtype CSN a = CSN a

-- |
-- An affine coordinate system itself in coordinate-system-neutral form.
--
type CoordinateSystem = CSN (Matrix Double)

instance (AffineTransformable a,Lerpable a) => Lerpable (CSN a) where
    lerp u (a,b) = modifyCSN world_coordinates (lerp u) $ pairCSN (a,b)

instance (AffineTransformable a,Eq a) => Eq (CSN a) where
    (CSN a) == (CSN b) = a == b

world_coordinates :: CoordinateSystem
world_coordinates = CSN $ identityMatrix 4

-- |
-- Store spatial data in a coordinate-system neutral way, using the current
-- affine transformation stack as the current coordinate system.
--
animToCSN :: (AffineTransformable a) => a -> AniM i o (CSN a)
animToCSN a = liftM CSN $ animTransform a

-- |
-- Retrieve coordinate-system neutral data, using the current
-- affine transformation stack as the current coordinate system.
--
animFromCSN :: (AffineTransformable a) => CSN a -> AniM i o a
animFromCSN (CSN a) = animInverseTransform a

-- |
-- Modify a value in the current coordinate system.
--
animModifyCSN :: (AffineTransformable a,AffineTransformable b) => (a -> b) -> CSN a -> AniM i o (CSN b)
animModifyCSN fn a = animToCSN =<< liftM fn (animFromCSN a)

-- |
-- Get a CSN in an arbitrary coordinate system.
--
fromCSN :: (AffineTransformable a) => CoordinateSystem -> CSN a -> a
fromCSN (CSN mat) (CSN a) = transform (matrixInverse mat) a

-- |
-- Store a value as a CSN based on an arbitrary coordinate system.
--
toCSN :: (AffineTransformable a) => CoordinateSystem -> a -> CSN a
toCSN (CSN mat) a = CSN $ transform mat a

-- |
-- Modify a CSN in an arbitrary coordinate system.
--
modifyCSN :: (AffineTransformable a,AffineTransformable b) => CoordinateSystem -> (a -> b) -> CSN a -> CSN b
modifyCSN cs fn = toCSN cs . fn . fromCSN cs

listCSN :: (AffineTransformable a) => [CSN a] -> CSN [a]
listCSN = CSN . map (\(CSN x) -> x)

unlistCSN :: (AffineTransformable a) => CSN [a] -> [CSN a]
unlistCSN (CSN x) = map CSN x

pairCSN :: (AffineTransformable a,AffineTransformable b) => (CSN a,CSN b) -> CSN (a,b)
pairCSN (CSN a,CSN b) = CSN (a,b)

unpairCSN :: (AffineTransformable a,AffineTransformable b) => CSN (a,b) -> (CSN a,CSN b)
unpairCSN (CSN (a,b)) = (CSN a,CSN b)

-- |
-- Get the current coordinate system.
--
animGetCoordinateSystem :: AniM i o CoordinateSystem
animGetCoordinateSystem = animToCSN $ identityMatrix 4

-- |
-- Store OpenGL IO action(s), to be executed in the current coordinate system.
-- These actions are stored and executed later, out-of-order, or not-at-all, which
-- is why the result of an animio issued IO action can not be retrieved from within
-- the AniM monad.
--
animio :: Point3D -> IO () -> AniM i o ()
animio p io = do p' <- animToCSN p
                 io' <- animToCSN io
                 animCSNio p' io'

animCSNio :: CSN Point3D -> CSN (IO ()) -> AniM i o ()
animCSNio p io = AniM $ modify (\x -> x { anim_io_actions = (AnimatedIOAction { animio_action = fromCSN world_coordinates io,
                                                                                animio_position = fromCSN world_coordinates p }) 
                                                                                    : anim_io_actions x })

-- |
-- An IO action embedded in the AniM monad.
--
unsafeAnimIO :: IO a -> AniM i o a
unsafeAnimIO io = AniM $ liftIO io

animTime :: AniM i o Time
animTime = AniM $ gets anim_time

animDeltaTime :: AniM i o Time
animDeltaTime = AniM $ gets anim_delta_time