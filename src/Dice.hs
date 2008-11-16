
module Dice (roll)
    where

import Control.Monad.State
import DB
import RandomUtils
import Control.Monad.Random

roll :: (DBReadable db) => [a] -> db a
roll xs = liftM (pick xs) getRandom
