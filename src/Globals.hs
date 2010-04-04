module Globals
    (Globals(..),
     default_globals)
    where

import RSAGL.Types

data Globals = Globals {
    global_planar_camera_distance :: RSdouble,
    global_sky_on :: Bool }

default_globals :: Globals
default_globals = Globals {
    global_planar_camera_distance = 5.0,
    global_sky_on = True }

