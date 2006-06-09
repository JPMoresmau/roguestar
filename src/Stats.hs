module Stats (generateStats) 
    where

import Dice
import StatsData
import DB

--
-- Randomly generate 1 statistic.
--
generate1Stat :: Integer -> Integer -> DB Integer
generate1Stat average deviation = do dieRoll <- (deviation `d` 3)
				     return (dieRoll + average - 2*deviation)

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
				       return Stats { str = new_str,
						      dex = new_dex,
						      con = new_con,
						      int = new_int,
						      per = new_per,
						      cha = new_cha,
						      mind = new_mind }