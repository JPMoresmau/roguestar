\section{The Roguestar Animation Arrow}

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving, Arrows, MultiParamTypeClasses #-}

module Animation
    (RSAnimA,
     RSAnimA1,
     RSAnimAX,
     RoguestarAnimationObject,
     newRoguestarAnimationObject,
     runRoguestarAnimationObject,
     driverGetAnswerA,
     driverGetTableA,
     printTextA,
     printTextOnce,
     debugA,
     debugOnce,
     printMenuItemA,
     printMenuA,
     clearPrintTextA,
     clearPrintTextOnce,
     libraryA,
     libraryPointAtCamera,
     blockContinue,
     requestPrintTextMode,
     readGlobal)
    where

import RSAGL.FRP
import RSAGL.CoordinateSystems
import RSAGL.Scene hiding (std_scene_layer_hud,std_scene_layer_cockpit,std_scene_layer_local,std_scene_layer_infinite)
import RSAGL.AnimationExtras
import Control.Monad.State
import Control.Arrow
import Control.Arrow.Transformer hiding (lift)
import Control.Arrow.Transformer as Arrow
import Control.Arrow.Transformer.State as StateArrow
import Control.Arrow.Operations
import Driver
import Tables
import Control.Concurrent.MVar
import RSAGL.Time
import System.IO
import PrintText
import Models.Library
import Models.LibraryData
import Quality
import Data.Maybe
import Keymaps.Keymaps
import Keymaps.CommonKeymap
import Actions
import Data.List
import Data.Ord
import Strings
import Actions
import Globals
import Data.IORef

data AnimationState = AnimationState {
    animstate_scene_accumulator :: SceneAccumulator IO,
    animstate_globals :: IORef Globals,
    animstate_driver_object :: DriverObject,
    animstate_print_text_object :: PrintTextObject,
    animstate_library :: Library,
    animstate_block_continue :: Bool,
    animstate_print_text_mode :: PrintTextMode }

instance CoordinateSystemClass AnimationState where
    getCoordinateSystem = getCoordinateSystem . animstate_scene_accumulator
    storeCoordinateSystem cs as = as { 
        animstate_scene_accumulator = storeCoordinateSystem cs $ animstate_scene_accumulator as }

instance ScenicAccumulator AnimationState IO where
    accumulateScene sl so as = as { 
        animstate_scene_accumulator = accumulateScene sl so $ animstate_scene_accumulator as }

newtype IOGuard a = IOGuard { runIOGuard :: IO a } deriving (Functor,Monad)

type RSAnimAX k t i o j p = FRPX k t i o (StateArrow AnimationState (Kleisli IOGuard)) j p
type RSAnimA t i o j p = RSAnimAX Threaded t i o j p
type RSAnimA1 i o j p = RSAnimAX () () i o j p

type RSAnimA_ j p = StateArrow AnimationState (Kleisli IOGuard) j p

newtype RoguestarAnimationObject = RoguestarAnimationObject {
    rso_arrow :: MVar (FRPProgram (StateArrow AnimationState (Kleisli IOGuard)) () Camera) }

newRoguestarAnimationObject :: RSAnimA1 () Camera () Camera -> IO RoguestarAnimationObject
newRoguestarAnimationObject rs_anim = 
    liftM RoguestarAnimationObject $ newMVar $ newFRP1Program rs_anim

runRoguestarAnimationObject :: Library -> IORef Globals -> DriverObject -> PrintTextObject -> RoguestarAnimationObject -> IO Scene
runRoguestarAnimationObject lib globals_ref driver_object print_text_object rso =
    do old_rso_program <- takeMVar $ rso_arrow rso
       t <- getTime
       ((result_camera,new_rso_program),result_animstate) <- runIOGuard $ (runKleisli $ StateArrow.runState $ updateFRPProgram old_rso_program) (((),t),
           AnimationState {
               animstate_globals = globals_ref,
	       animstate_scene_accumulator = null_scene_accumulator,
	       animstate_driver_object = driver_object,
	       animstate_print_text_object = print_text_object,
	       animstate_library = lib,
	       animstate_block_continue = False,
	       animstate_print_text_mode = Limited })
       putMVar (rso_arrow rso) new_rso_program
       when (not $ animstate_block_continue result_animstate) $ executeContinueAction $ ActionInput globals_ref driver_object print_text_object
       setPrintTextMode print_text_object $ animstate_print_text_mode result_animstate
       assembleScene (SceneLayerInfo (stdSceneLayers result_camera) (stdLightSourceLayerTransform $ stdSceneLayers result_camera)) $ 
		     animstate_scene_accumulator result_animstate

ioA :: (j -> IO p) -> RSAnimAX any t i o j p
ioA action = Arrow.lift $ ioA_ action

ioA_ :: (j -> IO p) -> RSAnimA_ j p
ioA_ action = proc j -> Arrow.lift $ Kleisli (\x -> IOGuard $ action x) -< j

driverGetAnswerA :: RSAnimAX any t i o String (Maybe String)
driverGetAnswerA = proc query ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioA (\(driver_object_,query_) -> driverGetAnswer driver_object_ query_) -< (driver_object,query)

driverGetTableA :: RSAnimAX any t i o (String,String) (Maybe RoguestarTable)
driverGetTableA = proc query ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioA (\(driver_object_,(the_table_name,the_table_id)) -> 
           driverGetTable driver_object_ the_table_name the_table_id) -< (driver_object,query)

printTextA :: RSAnimAX any t i o (Maybe (TextType,String)) ()
printTextA = Arrow.lift printTextA_

printTextOnce :: RSAnimAX any t i o (Maybe (TextType,String)) ()
printTextOnce = onceA printTextA_ 

printTextA_ :: RSAnimA_ (Maybe (TextType,String)) ()
printTextA_ = proc pt_data ->
    do print_text_object <- arr animstate_print_text_object <<< fetch -< ()
       ioA_ (\(print_text_object,x) -> case x of
            Nothing -> return ()
	    Just (pt_type,pt_string) -> printText print_text_object pt_type pt_string) -< (print_text_object,pt_data)

debugA :: RSAnimAX any t i o (Maybe String) ()
debugA = Arrow.lift debugA_

debugA_ :: RSAnimA_ (Maybe String) ()
debugA_ = Arrow.lift $ Kleisli $ maybe (return ()) (IOGuard . hPutStrLn stderr)

debugOnce :: RSAnimAX any t i o (Maybe String) ()
debugOnce = onceA debugA_

actionNameToKeysA :: String -> RSAnimAX any t i o () [String]
actionNameToKeysA action_name = Arrow.lift $ proc () ->
    do animstate <- fetch -< ()
       let action_input = ActionInput (animstate_globals animstate)
                                      (animstate_driver_object animstate)
                                      (animstate_print_text_object animstate)
       app -< (Arrow.lift $ Kleisli $ const $ IOGuard $ actionNameToKeys action_input common_keymap action_name,())

printMenuA :: [String] -> RSAnimAX any t i o () ()
printMenuA = foldr (>>>) (arr id) . map printMenuItemA

printMenuItemA :: String -> RSAnimAX any t i o () ()
printMenuItemA action_name = proc () ->
    do keys <- actionNameToKeysA action_name -< ()
       printTextA -< fmap (\s -> (Query,s ++ " - " ++ hrstring action_name)) $ listToMaybe $ sortBy (comparing length) keys

clearPrintTextA :: RSAnimAX any t i o () ()
clearPrintTextA = Arrow.lift clearPrintText_ <<< arr (const $ Just ())

clearPrintTextOnce :: RSAnimAX any t i o () ()
clearPrintTextOnce = onceA clearPrintText_ <<< arr (const $ Just ())

clearPrintText_ :: RSAnimA_ (Maybe ()) ()
clearPrintText_ = proc i ->
    do print_text_object <- arr (animstate_print_text_object) <<< fetch -< ()
       app -< maybe (arr $ const (),()) (const (Arrow.lift $ Kleisli $ const $ IOGuard $ clearOutputBuffer print_text_object,())) i

onceA :: StateArrow AnimationState (Kleisli IOGuard) (Maybe j) p -> RSAnimAX any t i o (Maybe j) p
onceA actionA = frp1Context onceA_
    where onceA_ = proc j -> 
              do p <- Arrow.lift actionA -< j
	         switchTerminate -< (if isJust j then (Just $ arr (const p)) else Nothing,p)

libraryA :: RSAnimAX any t i o (SceneLayer,LibraryModel) ()
libraryA = proc (layer,lm) ->
    do lib <- arr animstate_library <<< fetch -< ()
       accumulateSceneA -< (layer,sceneObject $ lookupModel lib lm Good)

libraryPointAtCamera :: RSAnimAX any t i o (SceneLayer,LibraryModel) ()
libraryPointAtCamera = proc (layer,lm) ->
    do lib <- arr animstate_library <<< fetch -< ()
       pointAtCameraA -< (layer,lookupModel lib lm Good)

blockContinue :: RSAnimAX any t i o Bool ()
blockContinue = Arrow.lift $ proc b ->
    do animstate <- fetch -< ()
       store -< animstate { animstate_block_continue = animstate_block_continue animstate || b }

requestPrintTextMode :: RSAnimAX any t i o PrintTextMode ()
requestPrintTextMode = Arrow.lift $ proc s ->
    do animstate <- fetch -< ()
       store -< animstate { animstate_print_text_mode = case (animstate_print_text_mode animstate,s) of
	   (_,Disabled) -> Disabled
	   (Limited,Unlimited) -> Unlimited
           (m,_) -> m }

readGlobal :: (Globals -> g) -> RSAnimAX any t i o () g
readGlobal f = proc () ->
    do globals_ref <- arr animstate_globals <<< fetch -< ()
       ioA (\globals_ref_ -> liftM f $ readIORef globals_ref_) -< globals_ref
\end{code}
