-- |
-- Avoid using globals if possible, but they're needed to communicate from the Actions to the rendering threads.
-- The rendering threads can't write to these, as idealy the rendering threads don't have side effects outside of their own thread state.
module Globals
    (Globals(..),
     default_globals)
    where

data Globals = Globals {
    global_planar_camera_distance :: Double }

default_globals :: Globals
default_globals = Globals {
    global_planar_camera_distance = 3.0 }

