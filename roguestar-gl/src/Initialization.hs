{-# LANGUAGE Arrows, OverloadedStrings #-}

module Initialization
    (initialize,
     Initialization(..))
    where

import System.IO
import PrintText
import Data.Maybe
import Control.Monad
import Actions
import Keymaps.Keymaps
import CommandLine
import Keymaps.BuiltinKeymaps
import RenderingControl
import Driver
import Animation
import RSAGL.Scene
import Models.Library
import System.Timeout
import System.Exit
import Globals
import Control.Concurrent.STM
import Control.Concurrent
import Statistics
import Config
import DrawString

data Initialization = Initialization {
    init_opts :: CommandLineOptions,
    init_keymap :: Keymap,
    init_scene_var :: TVar (Maybe Scene),
    init_globals :: Globals,
    init_driver_object :: DriverObject,
    init_print_text_object :: PrintTextObject,
    init_animation_object :: RoguestarAnimationObject,
    init_library :: Library,
    init_display_statistics :: Statistics,
    init_scene_statistics :: Statistics }

initialize :: DrawString -> [String] -> IO Initialization
initialize draw_strategy args =
    do let opts = parseCommandLine args
       let keymap = findKeymapOrDefault $ keymap_name opts
       scene_var <- newTVarIO Nothing
       globals <- defaultGlobals
       driver_object <- newDriverObject
       print_text_object <- newPrintTextObject draw_strategy
       animation_object <- newRoguestarAnimationObject mainAnimationLoop
       lib <- newLibrary
       display_statistics <- newStatistics "rendering"
       scene_statistics <- newStatistics "scene"
       return $ Initialization {
           init_opts = opts,
           init_keymap = keymap,
           init_scene_var = scene_var,
           init_globals = globals,
           init_driver_object = driver_object,
           init_print_text_object = print_text_object,
           init_animation_object = animation_object,
           init_library = lib,
           init_display_statistics = display_statistics,
           init_scene_statistics = scene_statistics }

