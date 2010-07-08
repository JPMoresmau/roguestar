module Globals
    (Globals(..),
     defaultGlobals)
    where

import RSAGL.Math.Types
import Quality
import Control.Concurrent.STM
import Control.Applicative

data Globals = Globals {
    global_planar_camera_distance :: TVar RSdouble,
    global_sky_on :: TVar Bool,
    global_quality_setting :: TVar Quality,
    global_should_quit :: TVar Bool }

defaultGlobals :: IO Globals
defaultGlobals = Globals <$>
    newTVarIO 5.0 <*>
    newTVarIO True <*>
    newTVarIO Poor <*>
    newTVarIO False

