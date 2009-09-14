{-# LANGUAGE Arrows #-}
module EventUtils
    (recentAttack)
    where

import VisibleObject
import Animation
import Control.Arrow
import RSAGL.FRP
import Data.Maybe

-- | Indicates the most recent time at which the specified creature performed an attack, in thread time.
recentAttack :: VisibleObjectReference -> RSAnimAX k (Maybe Integer) i o () (Maybe Time)
recentAttack obj = proc () ->
    do state <- driverGetAnswerA -< "state"
       count <- driverGetAnswerA -< "action-count"
       who <- getVisibleObject Attacker -< ()
       am_i <- getVisibleObject obj -< ()
       t <- threadTime -< ()
       arr snd <<< accumulate (Nothing,Nothing) (\(is_me,new_state,new_count,new_t) old ->
                      if new_state `elem` (map Just ["attack-event","miss-event","sunder-event","disarm-event"]) && 
                         new_count /= fst old && is_me
                      then (new_count,Just new_t) else old) -< (isJust who && who == am_i,state,count,t)

