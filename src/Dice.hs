
module Dice (roll)
    where

import Control.Monad.State
import DB
import RandomUtils

roll :: [a] -> DB a
roll xs = liftM (pick xs) dbNextRandomInteger
