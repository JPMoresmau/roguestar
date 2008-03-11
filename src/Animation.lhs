\section{The Roguestar Animation Arrow}

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving, Arrows #-}

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
     printMenuItemOnce,
     printMenuOnce,
     clearPrintTextOnce,
     libraryA)
    where

import RSAGL.FRP
import RSAGL.CoordinateSystems
import RSAGL.Scene
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
import Keymaps
import DefaultKeymap
import Actions
import Data.List
import Data.Ord
import Strings

data AnimationState = AnimationState {
    animstate_scene_accumulator :: SceneAccumulator,
    animstate_driver_object :: DriverObject,
    animstate_print_text_object :: PrintTextObject,
    animstate_library :: Library }

instance CoordinateSystemClass AnimationState where
    getCoordinateSystem = getCoordinateSystem . animstate_scene_accumulator
    storeCoordinateSystem cs as = as { 
        animstate_scene_accumulator = storeCoordinateSystem cs $ animstate_scene_accumulator as }

instance ScenicAccumulator AnimationState where
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

runRoguestarAnimationObject :: Library -> DriverObject -> PrintTextObject -> RoguestarAnimationObject -> IO Scene
runRoguestarAnimationObject lib driver_object print_text_object rso =
    do old_rso_program <- takeMVar $ rso_arrow rso
       t <- getTime
       ((result_camera,new_rso_program),result_animstate) <- runIOGuard $ (runKleisli $ StateArrow.runState $ updateFRPProgram old_rso_program) (((),t),
           AnimationState {
               animstate_scene_accumulator = null_scene_accumulator,
	       animstate_driver_object = driver_object,
	       animstate_print_text_object = print_text_object,
	       animstate_library = lib })
       putMVar (rso_arrow rso) new_rso_program
       assembleScene result_camera $ animstate_scene_accumulator result_animstate

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
       let action_input = ActionInput (animstate_driver_object animstate)
                                      (animstate_print_text_object animstate)
       app -< (Arrow.lift $ Kleisli $ const $ IOGuard $ actionNameToKeys action_input default_keymap action_name,())

printMenuOnce :: [String] -> RSAnimAX any t i o () ()
printMenuOnce = foldr (>>>) (arr id) . map printMenuItemOnce

printMenuItemOnce :: String -> RSAnimAX any t i o () ()
printMenuItemOnce action_name = proc () ->
    do keys <- actionNameToKeysA action_name -< ()
       printTextOnce -< fmap (\s -> (Query,s ++ " - " ++ hrstring action_name)) $ listToMaybe $ sortBy (comparing length) keys

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
\end{code}
