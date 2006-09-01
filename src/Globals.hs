module Globals
    (RoguestarGlobals(..),
     roguestar_globals_0,
     RoguestarEngineState(..))
    where

import Data.IORef
import Translation
import PrintTextData
import Quality
import Tables
import DefaultKeymap

data RoguestarEngineState = RoguestarEngineState { restate_tables :: [RoguestarTable], restate_answers :: [(String,String)] }

-- |
-- Some nasty, nasty global variables that we can't seem to live without.
--
-- global_quality -- the graphics quality setting (this should never change -- it's set by a command line option)
-- global_display_func -- the OpenGL display function (change this using the accessor function FIXME no accessor function yet)
-- global_text_output_buffer -- text that has been printed to the screen (don't touch unless you're PrintText.hs)
-- global_text_output_mode -- whether we're showing an entire screen of text or just a few lines (or nothing) (don't touch unless you're PrintText.hs)
-- global_engine_input_lines -- text/protocol input from the engine (don't touch unless you're Driver.hs)
-- global_engine_input_line_fragment -- the current string being read (don't touch unless you're Driver.hs)
-- global_engine_output_lines -- lines that have already been sent to stdout (don't touch unless you're Driver.hs), used to ensure that we don't send the same request many times
-- global_engine_state -- raw state information pulled from the engine (use accessor functions in Driver.hs)
-- global_language -- language (anyone can read, only main function should write based on command line arguments)
-- global_keymap -- mapping from keystrokes to action names, only (don't touch unless you're Main.hs)
-- global_user_input -- input from the user typing (don't touch unless you're Main.hs)
-- global_dones -- number of "done" lines recieved from the engine, used to track turn changes.
--
data RoguestarGlobals = RoguestarGlobals {
					  global_quality :: Quality,
					  global_display_func :: (IORef RoguestarGlobals) -> IO (),
					  global_text_output_buffer :: [(TextType,String)], -- in reverse order for ease of appending
					  global_text_output_mode :: PrintTextMode,
					  global_engine_input_lines :: [String], -- in reverse order for ease of appending (but each string is in forward order)
					  global_engine_input_line_fragment :: String, -- in reverse order for easy of appending
					  global_engine_output_lines :: [String],
					  global_engine_state :: RoguestarEngineState,
					  global_language :: Language,
					  global_keymap :: [(String,String)], -- map of keystrokes to action names
					  global_user_input :: String, -- in normal order
					  global_dones :: Integer
					 }

-- |
-- Default starting value for all globals.
--
roguestar_globals_0 :: RoguestarGlobals
roguestar_globals_0 = RoguestarGlobals {
					global_quality = Good,
					global_display_func = \_ -> return (),
					global_text_output_buffer = [],
					global_text_output_mode = Unlimited,
					global_engine_input_lines = [],
					global_engine_input_line_fragment = "",
					global_engine_output_lines = [],
					global_engine_state = RoguestarEngineState { restate_tables = [], restate_answers = [] },
					global_language = English,
					global_keymap = default_keymap,
					global_user_input = [],
					global_dones = 0
				       }
