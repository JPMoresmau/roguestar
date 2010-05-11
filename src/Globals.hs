module Globals
    (Globals(..),
     default_globals)
    where

import RSAGL.Types
import Quality

data Globals = Globals {
    global_planar_camera_distance :: RSdouble,
    global_sky_on :: Bool,
    global_quality_setting :: Quality }

default_globals :: Globals
default_globals = Globals {
    global_planar_camera_distance = 5.0,
    global_sky_on = True,
    global_quality_setting = Poor }

