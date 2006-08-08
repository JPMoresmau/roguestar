module Globals
    (RoguestarGlobals(..))
    where

import Quality

-- |
-- Some nasty, nasty global variables that can't live without.
--
data RoguestarGlobals = RoguestarGlobals {
					  global_quality :: Quality,
					  global_display_func :: IO ()
					 }