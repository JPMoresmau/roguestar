module Logging
    (log_database,
     log_plane,
     log_travel,
     log_turns,
     module System.Log.Logger)
    where

import System.Log.Logger

log_database :: String
log_database = "engine.database"

log_plane :: String
log_plane = "engine.plane"

log_travel :: String
log_travel = "engine.travel"

log_turns :: String
log_turns = "engine.turns"

