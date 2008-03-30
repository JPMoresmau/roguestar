
module Stats (generateStats) 
    where

import Dice
import StatsData
import DB

--
-- Randomly generate 1 statistic.
--
generate1Stat :: Integer -> Integer -> DB Integer
generate1Stat minimal range = roll $ concat [[minimal..minimal+i] | i <- [0..range]]

--
-- Randomly generate statistics.
--
generateStats :: Stats -> Stats -> DB Stats
generateStats minimums ranges = 
    do new_str <- generate1Stat (str minimums) (str ranges)
       new_dex <- generate1Stat (dex minimums) (dex ranges)
       new_con <- generate1Stat (con minimums) (con ranges)
       new_int <- generate1Stat (int minimums) (int ranges)
       new_per <- generate1Stat (per minimums) (per ranges)
       new_cha <- generate1Stat (cha minimums) (cha ranges)
       new_mind <- generate1Stat (mind minimums) (mind ranges)
       return Stats { strength = new_str,
                      dexterity = new_dex,
                      constitution = new_con,
                      intelligence = new_int,
                      perception = new_per,
                      charisma = new_cha,
                      mindfulness = new_mind }
