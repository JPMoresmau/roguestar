{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, Arrows, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, ExistentialQuantification, Rank2Types, OverloadedStrings #-}

module Animation
    (RSAnimAX,
     RoguestarAnimationObject,
     newRoguestarAnimationObject,
     runRoguestarAnimationObject,
     driverGetAnswerA,
     driverGetTableA,
     printTextA,
     printTextOnce,
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
     readGlobal,
     randomA)
    where

import RSAGL.Math
import RSAGL.FRP
import RSAGL.Scene hiding (std_scene_layer_hud,std_scene_layer_cockpit,std_scene_layer_local,std_scene_layer_infinite)
import RSAGL.Animation
import RSAGL.Auxiliary.RecombinantState
import Control.Monad.State
import Control.Arrow
import Control.Arrow.Operations
import Driver
import Tables
import System.IO
import System.Random
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
import Globals
import Data.IORef
import qualified Data.ByteString as B

data AnimationState = AnimationState {
    animstate_scene_accumulator :: SceneAccumulator IO,
    animstate_globals :: IORef Globals,
    animstate_driver_object :: FrozenDriver,
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

instance RecombinantState AnimationState where
    type SubState AnimationState = AnimationState
    clone old = old { animstate_scene_accumulator = clone $ animstate_scene_accumulator old }
    recombine old new = old { 
        animstate_scene_accumulator = recombine (animstate_scene_accumulator old) (animstate_scene_accumulator new),
        animstate_block_continue = animstate_block_continue old || animstate_block_continue new,
        animstate_print_text_mode = animstate_print_text_mode old `mergePrintTextModes` animstate_print_text_mode new }

-- | The FRP arrows for roguestar animations.
type RSAnimAX k t i o = FRPX k AnimationState t i o

instance (CoordinateSystemClass csc) => AffineTransformable (FRPX k csc t i o j p) where
    transform m actionA = proc x -> transformA actionA -< (Affine $ transform m,x)

newtype RoguestarAnimationObject = RoguestarAnimationObject (FRPProgram AnimationState () SceneLayerInfo)

newRoguestarAnimationObject :: RSAnimAX () () () SceneLayerInfo () SceneLayerInfo -> IO RoguestarAnimationObject
newRoguestarAnimationObject rs_anim = 
    liftM RoguestarAnimationObject $ newFRP1Program rs_anim

runRoguestarAnimationObject :: Library -> IORef Globals -> DriverObject -> PrintTextObject -> RoguestarAnimationObject -> IO Scene
runRoguestarAnimationObject lib globals_ref driver_object print_text_object (RoguestarAnimationObject rso) =
    do frozen_driver_object <- freezeDriver driver_object
       let anim_state = AnimationState {
               animstate_globals = globals_ref,
	       animstate_scene_accumulator = null_scene_accumulator,
	       animstate_driver_object = frozen_driver_object,
	       animstate_print_text_object = print_text_object,
	       animstate_library = lib,
	       animstate_block_continue = False,
	       animstate_print_text_mode = Limited }
       (result_scene_layer_info,result_animstate) <- updateFRPProgram Nothing ((),anim_state) rso
       when (not $ animstate_block_continue result_animstate) $ executeContinueAction $ ActionInput globals_ref driver_object print_text_object
       setPrintTextMode print_text_object $ animstate_print_text_mode result_animstate
       assembleScene result_scene_layer_info $ animstate_scene_accumulator result_animstate

-- | Request an answer from the engine.  This will return 'Nothing' until the answer arrives, which may never happen.
driverGetAnswerA :: RSAnimAX any t i o B.ByteString (Maybe B.ByteString)
driverGetAnswerA = proc query ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioAction (\(driver_object_,query_) -> getAnswer driver_object_ query_) -< (driver_object,query)

-- | Request a data table from the engine.  This will return 'Nothing' until the entire table arrives, which may never happen.
driverGetTableA :: RSAnimAX any t i o (B.ByteString,B.ByteString) (Maybe RoguestarTable)
driverGetTableA = proc query ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioAction (\(driver_object_,(the_table_name,the_table_id)) -> 
                 getTable driver_object_ the_table_name the_table_id) -< (driver_object,query)

-- | Print a line of text to the game console.  This will print exactly once.
-- Accepts 'Nothing' and prints once immediately when a value is supplied.
printTextOnce :: RSAnimAX k t i o (Maybe (TextType,B.ByteString)) ()
printTextOnce = onceA printTextA 

printTextA :: RSAnimAX k t i o (Maybe (TextType,B.ByteString)) ()
printTextA = proc pt_data ->
    do print_text_object <- arr animstate_print_text_object <<< fetch -< ()
       ioAction (\(print_text_object,x) -> case x of
            Nothing -> return ()
	    Just (pt_type,pt_string) -> printText print_text_object pt_type pt_string) -< (print_text_object,pt_data)

-- | Number of dones.  (A done is a message from the engine that an change has occured in the game world.)
donesA :: RSAnimAX k t i o () Integer
donesA = proc () ->
    do driver_object <- arr animstate_driver_object <<< fetch -< ()
       ioAction driverDones -< thawDriver driver_object

-- | Print a debugging message to 'stderr'.  This will print on every frame of animation.
debugA :: RSAnimAX k t i o (Maybe B.ByteString) ()
debugA = ioAction (maybe (return ()) (B.hPutStrLn stderr))

-- | Print a debugging message to 'stderr'.  This will print exactly once.
debugOnce :: RSAnimAX k t i o (Maybe B.ByteString) ()
debugOnce = onceA debugA

-- | Get a list of keystrokes that correspond to the specified action, that are valid on the current frame of animation.
-- This can be used to display a menu that correctly indicates what keystroke to press for a given action.
actionNameToKeysA :: B.ByteString -> RSAnimAX any t i o () [B.ByteString]
actionNameToKeysA action_name = proc () ->
    do animstate <- fetch -< ()
       let action_input = ActionInput (animstate_globals animstate)
                                      (thawDriver $ animstate_driver_object animstate)
                                      (animstate_print_text_object animstate)
       ioAction id -< actionNameToKeys action_input common_keymap action_name

-- | Print a menu using 'printMenuItemA'
printMenuA :: [B.ByteString] -> RSAnimAX any t i o () ()
printMenuA = foldr (>>>) (arr id) . map printMenuItemA

-- | Print a single menu item including it's keystroke.
printMenuItemA :: B.ByteString -> RSAnimAX any t i o () ()
printMenuItemA action_name = proc () ->
    do keys <- actionNameToKeysA action_name -< ()
       printTextA -< fmap (\s -> (Query,s `B.append` " - " `B.append` hrstring action_name)) $ listToMaybe $ sortBy (comparing B.length) keys

-- | Clear all printed text once.  This begins a new clean segment of printed text.
clearPrintTextOnce :: RSAnimAX k t i o () ()
clearPrintTextOnce = onceA clearPrintTextA <<< arr (const $ Just ())

clearPrintTextA :: RSAnimAX k t i o (Maybe ()) ()
clearPrintTextA = proc i ->
    do print_text_object <- arr (animstate_print_text_object) <<< fetch -< ()
       ioAction id -< when (isJust i) $ clearOutputBuffer print_text_object

-- | Do an action exactly once.
onceA :: (forall x y. RSAnimAX () () x y (Maybe j) p) -> RSAnimAX k t i o (Maybe j) p
onceA actionA = frp1Context onceA_
    where onceA_ = proc j -> 
              do p <- actionA -< j
	         switchTerminate -< (if isJust j then (Just $ arr (const p)) else Nothing,p)

-- | Display a library model.
libraryA :: RSAnimAX any t i o (SceneLayer,LibraryModel) ()
libraryA = proc (layer,lm) ->
    do lib <- arr animstate_library <<< fetch -< ()
       accumulateSceneA -< (layer,sceneObject $ lookupModel lib lm Poor)

-- | Display a library model that remains oriented toward the camera.
libraryPointAtCamera :: RSAnimAX any t i o (SceneLayer,LibraryModel) ()
libraryPointAtCamera = proc (layer,lm) ->
    do lib <- arr animstate_library <<< fetch -< ()
       pointAtCameraA -< (layer,lookupModel lib lm Poor)

-- | Prevent the engine from auto-continuing.  When the engine is in a snapshot state, 
-- the client will automatically ask it to step forward either to the next snapshot or
-- the player's turn.  This delays the continue action until some animation or
-- text finishes printing.
blockContinue :: RSAnimAX k t i o Bool ()
blockContinue = proc b ->
    do animstate <- fetch -< ()
       store -< animstate { animstate_block_continue = animstate_block_continue animstate || b }

-- | Change the 'PrintTextMode'.  If multiple calls are made to 'requestPrintTextMode', then
-- 'Disabled' takes precedence over all others, while 'Unlimited' takes precedence over 'Limited'.
requestPrintTextMode :: RSAnimAX k t i o PrintTextMode ()
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
readGlobal :: (Globals -> g) -> RSAnimAX any t i o () g
readGlobal f = proc () ->
    do globals_ref <- arr animstate_globals <<< fetch -< ()
       ioAction (\globals_ref_ -> liftM f $ readIORef globals_ref_) -< globals_ref

-- | Get a bounded random value, as 'randomRIO'.  A new value is pulled for each frame of animation.
randomA :: (Random a) => RSAnimAX any t i o (a,a) a
randomA = ioAction randomRIO
