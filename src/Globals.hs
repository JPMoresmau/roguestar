module Globals
    (RoguestarGlobals(..),
     roguestar_globals_0,
     RoguestarTable(..),
     RoguestarEngineState(..),
     tableSelect)
    where

import Data.List
import Data.IORef
import Translation
import PrintTextData
import Quality

data RoguestarTable = RoguestarTable { table_name, table_id :: String, table_header :: [String], table_data :: [[String]] }

data RoguestarEngineState = RoguestarEngineState { restate_tables :: [RoguestarTable], restate_answers :: [(String,String)] }

-- |
-- Select from a table, like the SQL select statement.
-- For example:
-- tableSelect people ["name","sex","phone-number"] = [["bob","male","123-4567"],["susan","female","987-6543"]]
-- If a given header is not in the table, lists "???" as the value.
--
tableSelect :: RoguestarTable -> [String] -> [[String]]
tableSelect table headers = let header_indices = map (\x -> elemIndex x $ table_header table) headers
				in map (rowSelect header_indices) $ table_data table

rowSelect :: [Maybe Int] -> [String] -> [String]
rowSelect (Nothing:more) row = "???" : rowSelect more row
rowSelect (Just x:more) row = (row !! x) : rowSelect more row
rowSelect [] _ = []

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
-- global_read_mvar -- the MVar containing the last character read by the driverReadThread (don't touch unless you're Driver.hs)
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
					  global_translator :: [String] -> String
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
					global_translator = tr English
				       }
