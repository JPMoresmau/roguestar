module Animation
    (module AnimationCore,
     module AnimationAux,
     runAnimation,
     getCamera)
    where

import Globals
import Data.IORef
import Control.Monad
import AnimationCore hiding (unsafeAnimIO)
import AnimationAux
import Driver
import Camera
import Data.Maybe
import Math3D

runAnimation :: IORef RoguestarGlobals -> Animation i o -> i -> IO (Maybe o)
runAnimation globals_ref animation i =
    do camera_animation <- liftM global_camera_animation $ readIORef globals_ref
       camerapt <- liftM (modifyCSN world_coordinates camera_position . fromMaybe (error "runAnimation: No Camera (bug in roguestar-gl!)")) $ 
                         runAnimationWithContext (error "runAnimation: no point supplied (bug in roguestar-gl!)") camera_animation ()
       runAnimationWithContext camerapt animation i
       
getCamera :: IORef RoguestarGlobals -> IO (CSN Camera)
getCamera globals_ref =
    do camera_animation <- liftM global_camera_animation $ readIORef globals_ref
       liftM (fromMaybe (error "camera: No Camera (bug in roguestar-gl!)")) $ 
             runAnimationWithContext (error "runAnimation: no point supplied (bug in roguestar-gl!)") camera_animation ()
