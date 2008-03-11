
module Stats (generateStats) 
    where

import Dice
import StatsData
import DB

--
-- Randomly generate 1 statistic.
--
generate1Stat :: Integer -> Integer -> DB Integer
generate1Stat average deviation = roll $ concat [[average-i..average+i] | i <- [0..deviation]]

--
-- Randomly generate statistics.
--
generateStats :: Stats -> Stats -> DB Stats
generateStats averages deviations = do new_str <- generate1Stat (str averages) (str deviations)
				       new_dex <- generate1Stat (dex averages) (dex deviations)
				       new_con <- generate1Stat (con averages) (con deviations)
				       new_int <- generate1Stat (int averages) (int deviations)
				       new_per <- generate1Stat (per averages) (per deviations)
				       new_cha <- generate1Stat (cha averages) (cha deviations)
				       new_mind <- generate1Stat (mind averages) (mind deviations)
				       return Stats { strength = new_str,
						      dexterity = new_dex,
						      constitution = new_con,
						      intelligence = new_int,
						      perception = new_per,
						      charisma = new_cha,
						      mindfulness = new_mind }
