module StatsData
    (Stats(..),
     stats)
    where

-- |
-- Represents the seven roguestar creature statistics:
-- Strength (str)
-- Dexterity (dex)
-- Constitution (con)
-- Intelligence (int)
-- Perception (per)
-- Charisma (cha)
-- Mindfulness (min)
--

data Stats = Stats {str, dex, con, int, per, cha, mind :: Integer} deriving (Show, Read)

--
-- Used to generate a Stats object with all the same stats (i.e. stats 1 => Stats 1 1 1 1 1 1 1)
--

stats :: Integer -> Stats
stats x = (Stats {str=x, dex=x, con=x, int=x, per=x, cha=x, mind=x})