module Globals
    (RoguestarGlobals(..))
    where

import PrintTextData
import Quality

-- |
-- Some nasty, nasty global variables that can't live without.
--
data RoguestarGlobals = RoguestarGlobals {
					  global_quality :: Quality,
					  global_display_func :: IO (),
					  global_text_output_buffer :: [(TextType,String)], -- in reverse order for ease of appending
					  global_text_output_mode :: PrintTextMode
					 }