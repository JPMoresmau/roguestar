{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,
             Arrows,
             MultiParamTypeClasses,
             FlexibleInstances,
             TypeFamilies,
             ExistentialQuantification,
             Rank2Types,
             OverloadedStrings,
             UndecidableInstances #-}

module Animation
    (RSwitch,
     AnimationState,
     FRPModes,
     RoguestarModes,
     RoguestarAnimationObject,
     newRoguestarAnimationObject,
     runRoguestarAnimationObject,
     driverGetAnswerA,
     driverGetTableA,
     printTextA,
     printTextOnce,
     statusA,
     debugA,
     debugOnce,
     donesA,
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

import RSAGL.Math
import RSAGL.FRP
import RSAGL.Scene hiding (std_scene_layer_hud,
                           std_scene_layer_cockpit,
                           std_scene_layer_local,
                           std_scene_layer_infinite)
import RSAGL.Animation
import RSAGL.FRP.RecombinantState
import Control.Monad.State
import Control.Arrow
import Control.Arrow.Operations
import Driver
import Tables
import System.IO
import PrintText
import Models.Library
import Models.LibraryData
import Data.Maybe
import Keymaps.Keymaps
import Keymaps.CommonKeymap
import Actions
import Data.List
import Data.Ord
import Strings
import Globals
import PrintText
import PrintTextData
import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.Map as Map

data AnimationState = AnimationState {
    animstate_scene_accumulator :: SceneAccumulator IO,
    animstate_globals :: Globals,
    animstate_driver_object :: FrozenDriver,
    animstate_print_text_object :: PrintTextObject,
    animstate_status_lines :: Map.Map StatusField B.ByteString,
    animstate_library :: Library,
    animstate_block_continue :: Bool,
    animstate_print_text_mode :: PrintTextMode,
    animstate_suspended_stm_action :: STM () }

instance CoordinateSystemClass AnimationState where
    getCoordinateSystem = getCoordinateSystem . animstate_scene_accumulator
    storeCoordinateSystem cs as = as {
        animstate_scene_accumulator = storeCoordinateSystem cs $ animstate_scene_accumulator as }

instance ScenicAccumulator AnimationState IO where
    accumulateScene sl so as = as {
        animstate_scene_accumulator = accumulateScene sl so $ animstate_scene_accumulator as }

instance RecombinantState AnimationState where
    type SubState AnimationState = AnimationState
    clone old = old { animstate_scene_accumulator = clone $ animstate_scene_accumulator old }
    recombine old new = old {
        animstate_scene_accumulator = recombine (animstate_scene_accumulator old) (animstate_scene_accumulator new),
        animstate_block_continue = animstate_block_continue old || animstate_block_continue new,
        animstate_print_text_mode = animstate_print_text_mode old `mergePrintTextModes` animstate_print_text_mode new }

-- | The RogueStar switch type.
type RSwitch k t i o m = SimpleSwitch k t AnimationState i o m

instance (CoordinateSystemClass csc,StateOf m ~ csc) => AffineTransformable (FRP e m j p) where
    transform m actionA = proc x -> transformA actionA -< (Affine $ transform m,x)

type FRPModes m = (StateOf m,InputOutputOf m)
type RoguestarModes = (AnimationState,Enabled)

newtype RoguestarAnimationObject = RoguestarAnimationObject (FRPProgram AnimationState () SceneLayerInfo)

newRoguestarAnimationObject :: (forall e. FRP e (FRP1 AnimationState () SceneLayerInfo) () SceneLayerInfo) -> IO RoguestarAnimationObject
newRoguestarAnimationObject rs_anim =
    liftM RoguestarAnimationObject $ newFRP1Program rs_anim

runRoguestarAnimationObject :: Library ->
                               Globals ->
                               DriverObject ->
                               PrintTextObject ->
                               RoguestarAnimationObject ->
                               IO Scene
runRoguestarAnimationObject lib globals driver_object print_text_object
                            (RoguestarAnimationObject rso) =
    do frozen_driver_object <- atomically $ freezeDriver driver_object
       let anim_state = AnimationState {
               animstate_globals = globals,
               animstate_scene_accumulator = null_scene_accumulator,
               animstate_driver_object = frozen_driver_object,
               animstate_print_text_object = print_text_object,
               animstate_status_lines = Map.empty,
               animstate_library = lib,
               animstate_block_continue = False,
               animstate_print_text_mode = Limited,
               animstate_suspended_stm_action = return () }
       (result_scene_layer_info,result_animstate) <-
           updateFRPProgram Nothing ((),anim_state) rso
       atomically $
           do when (not $ animstate_block_continue result_animstate) $
                  executeContinueAction $
                      ActionInput globals driver_object print_text_object
              setPrintTextMode print_text_object $
                  animstate_print_text_mode result_animstate
              animstate_suspended_stm_action result_animstate
              setStatus print_text_object $
                  animstate_status_lines result_animstate
       assembleScene result_scene_layer_info $
           animstate_scene_accumulator result_animstate

-- | Request an answer from the engine.  This will return 'Nothing' until the
-- answer arrives, which may never happen.
driverGetAnswerA :: (StateOf m ~ AnimationState,
                     InputOutputOf m ~ Enabled) =>
                    FRP e m B.ByteString (Maybe B.ByteString)
driverGetAnswerA = proc query ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioAction (\(driver_object_,query_) ->
           atomically $ getAnswer driver_object_ query_) -<
               (driver_object,query)

-- | Request a data table from the engine.  This will return 'Nothing' until the entire table arrives, which may never happen.
driverGetTableA :: (StateOf m ~ AnimationState,
                    InputOutputOf m ~ Enabled) =>
                   FRP e m (B.ByteString,B.ByteString) (Maybe RoguestarTable)
driverGetTableA = proc query ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioAction (\(driver_object_,(the_table_name,the_table_id)) ->
                 atomically $
                     getTable driver_object_ the_table_name the_table_id) -<
                         (driver_object,query)

-- | Store an IO action and run it at the end of the frame.
suspendedSTMAction :: (FRPModel m, StateOf m ~ AnimationState) =>
                     (i -> STM ()) -> FRP e m i ()
suspendedSTMAction action = proc i ->
    do s <- fetch -< ()
       store -< s { animstate_suspended_stm_action =
           animstate_suspended_stm_action s >> action i }

-- | Print a line of text to the game console.  This will print exactly once.
-- Accepts 'Nothing' and prints once immediately when a value is supplied.
printTextOnce :: (FRPModel m, StateOf m ~ AnimationState) =>
                 FRP e m (Maybe (TextType,B.ByteString)) ()
printTextOnce = onceA printTextA

printTextA :: (FRPModel m, StateOf m ~ AnimationState) =>
              FRP e m (Maybe (TextType,B.ByteString)) ()
printTextA = proc pt_data ->
    do print_text_object <- arr animstate_print_text_object <<< fetch -< ()
       suspendedSTMAction (\(print_text_object,x) -> case x of
            Nothing -> return ()
            Just (pt_type,pt_string) ->
                printText print_text_object pt_type pt_string)
           -< (print_text_object,pt_data)

statusA :: (FRPModel m, StateOf m ~ AnimationState) =>
           FRP e m (Maybe (StatusField,B.ByteString)) ()
statusA = proc status_data ->
    do animstate <- fetch -< ()
       store -< case status_data of
           Just (field,status) -> animstate { animstate_status_lines =
               Map.insert field status $
                   animstate_status_lines animstate }
           Nothing -> animstate

-- | Number of dones.  (A done is a message from the engine that an change has occured in the game world.)
donesA :: (StateOf m ~ AnimationState,
           InputOutputOf m ~ Enabled) =>
          FRP e m () Integer
donesA = proc () ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioAction (atomically . driverDones) -< driver_object

-- | Print a debugging message to 'stderr'.  This will print on every frame of animation.
debugA :: (StateOf m ~ AnimationState,
           InputOutputOf m ~ Enabled) =>
          FRP e m (Maybe B.ByteString) ()
debugA = ioAction (maybe (return ()) (B.hPutStrLn stderr))

-- | Print a debugging message to 'stderr'.  This will print exactly once.
debugOnce :: (FRPModel m, StateOf m ~ AnimationState,
              InputOutputOf m ~ Enabled) =>
             FRP e m (Maybe B.ByteString) ()
debugOnce = onceA debugA

-- | Get a list of keystrokes that correspond to the specified action, that are
-- valid on the current frame of animation.  This can be used to display a menu
-- that correctly indicates what keystroke to press for a given action.
actionNameToKeysA :: (StateOf m ~ AnimationState,
                      InputOutputOf m ~ Enabled) =>
                     B.ByteString -> FRP e m () [B.ByteString]
actionNameToKeysA action_name = proc () ->
    do animstate <- fetch -< ()
       let action_input = ActionInput (animstate_globals animstate)
                                      (thawDriver $ animstate_driver_object animstate)
                                      (animstate_print_text_object animstate)
       ioAction id -< atomically (actionNameToKeys action_input
                                                   common_keymap
                                                   action_name)

-- | Print a menu using 'printMenuItemA'
printMenuA :: (FRPModel m, StateOf m ~ AnimationState,
               InputOutputOf m ~ Enabled) =>
              [B.ByteString] -> FRP e m () ()
printMenuA = foldr (>>>) (arr id) . map printMenuItemA

-- | Print a single menu item including it's keystroke.
printMenuItemA :: (FRPModel m, StateOf m ~ AnimationState,
                   InputOutputOf m ~ Enabled) =>
                  B.ByteString -> FRP e m () ()
printMenuItemA action_name = proc () ->
    do keys <- actionNameToKeysA action_name -< ()
       printTextA -< fmap (\s -> (Query,s `B.append` " - " `B.append` hrstring action_name)) $ listToMaybe $ sortBy (comparing B.length) keys

-- | Clear all printed text once.  This begins a new clean segment of printed text.
clearPrintTextOnce :: (FRPModel m, StateOf m ~ AnimationState) => FRP e m () ()
clearPrintTextOnce = onceA clearPrintTextA <<< arr (const $ Just ())

clearPrintTextA :: (FRPModel m, StateOf m ~ AnimationState) => FRP e m (Maybe ()) ()
clearPrintTextA = proc i ->
    do print_text_object <- arr (animstate_print_text_object) <<< fetch -< ()
       suspendedSTMAction id -<
           when (isJust i) $ clearOutputBuffer print_text_object

-- | Do an action exactly once.
onceA :: (FRPModel m) =>
    (forall n. (FRPModel n, StateOf n ~ StateOf m,
                InputOutputOf n ~ InputOutputOf m) =>
               FRP e n (Maybe j) p) ->
    FRP e m (Maybe j) p
onceA actionA = frp1Context onceA_
    where onceA_ = proc j ->
              do p <- actionA -< j
                 switchTerminate -< (if isJust j then (Just $ arr (const p)) else Nothing,p)

-- | Display a library model.
libraryA :: (StateOf m ~ AnimationState,LibraryModelSource lm,
             InputOutputOf m ~ Enabled) =>
            FRP e m (SceneLayer,lm) ()
libraryA = proc (layer,lm) ->
    do q <- readGlobal global_quality_setting -< ()
       lib <- arr animstate_library <<< fetch -< ()
       accumulateSceneA -< (layer,
           sceneObject $ lookupModel lib (toLibraryModel lm) q)

-- | Display a library model that remains oriented toward the camera.
libraryPointAtCamera :: (StateOf m ~ AnimationState,LibraryModelSource lm,
                         InputOutputOf m ~ Enabled) =>
                        FRP e m (SceneLayer,lm) ()
libraryPointAtCamera = proc (layer,lm) ->
    do q <- readGlobal global_quality_setting -< ()
       lib <- arr animstate_library <<< fetch -< ()
       pointAtCameraA -< (layer,lookupModel lib (toLibraryModel lm) q)

-- | Prevent the engine from auto-continuing.  When the engine is in a snapshot state, 
-- the client will automatically ask it to step forward either to the next snapshot or
-- the player's turn.  This delays the continue action until some animation or
-- text finishes printing.
blockContinue :: (StateOf m ~ AnimationState) => FRP e m Bool ()
blockContinue = proc b ->
    do animstate <- fetch -< ()
       store -< animstate { animstate_block_continue = animstate_block_continue animstate || b }

-- | Change the 'PrintTextMode'.  If multiple calls are made to 'requestPrintTextMode', then
-- 'Disabled' takes precedence over all others, while 'Unlimited' takes precedence over 'Limited'.
requestPrintTextMode :: (StateOf m ~ AnimationState) => FRP e m PrintTextMode ()
requestPrintTextMode = proc s ->
    do animstate <- fetch -< ()
       store -< animstate { animstate_print_text_mode = animstate_print_text_mode animstate `mergePrintTextModes` s }

mergePrintTextModes :: PrintTextMode -> PrintTextMode -> PrintTextMode
mergePrintTextModes _ Disabled = Disabled
mergePrintTextModes Disabled _ = Disabled
mergePrintTextModes Limited Unlimited = Unlimited
mergePrintTextModes Unlimited Limited = Unlimited
mergePrintTextModes m _ = m

-- | Read a global variable.
readGlobal :: (StateOf m ~ AnimationState,
               InputOutputOf m ~ Enabled) =>
              (Globals -> TVar g) -> FRP e m () g
readGlobal f = proc () ->
    do globals <- arr animstate_globals <<< fetch -< ()
       ioAction (\globals_ -> atomically $ readTVar $ f globals_) -< globals

