
module StatsData
    (Stats(..),
     StatisticsBlock(..),
     Statistic(..),
     stats,
     getStatistic,
     setStatistic)
    where

class StatisticsBlock a where
    str :: a -> Integer
    dex :: a -> Integer
    con :: a -> Integer
    int :: a -> Integer
    per :: a -> Integer
    cha :: a -> Integer
    mind :: a -> Integer

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

data Stats = Stats {strength, dexterity, constitution, intelligence, perception, charisma, mindfulness :: Integer} deriving (Show, Read)

instance StatisticsBlock Stats where
    str = strength
    dex = dexterity
    con = constitution
    int = intelligence
    per = perception
    cha = charisma
    mind = mindfulness
    
data Statistic = Strength
	       | Dexterity
	       | Constitution
	       | Intelligence
	       | Perception
	       | Charisma
	       | Mindfulness
	       deriving (Eq,Read,Show)

getStatistic :: StatisticsBlock a => Statistic -> a -> Integer
getStatistic Strength = str
getStatistic Dexterity = dex
getStatistic Constitution = con
getStatistic Intelligence = int
getStatistic Perception = per
getStatistic Charisma = cha
getStatistic Mindfulness = mind

setStatistic :: Statistic -> Integer -> Stats -> Stats
setStatistic Strength = setStr
setStatistic Dexterity = setDex
setStatistic Constitution = setCon
setStatistic Intelligence = setInt
setStatistic Perception = setPer
setStatistic Charisma = setCha
setStatistic Mindfulness = setMind

-- |
-- Used to generate a Stats object with all the same stats (i.e. stats 1 => Stats 1 1 1 1 1 1 1)
--

stats :: Integer -> Stats
stats x = (Stats {strength=x, dexterity=x, constitution=x, intelligence=x, perception=x, charisma=x, mindfulness=x})

-- |
-- Functions to modify a single stat in a Stats block.
--
setStr :: Integer -> Stats -> Stats
setStr x st = st { strength = x }

setDex :: Integer -> Stats -> Stats
setDex x st = st { dexterity = x }

setCon :: Integer -> Stats -> Stats
setCon x st = st { constitution = x }

setInt :: Integer -> Stats -> Stats
setInt x st = st { intelligence = x }

setPer :: Integer -> Stats -> Stats
setPer x st = st { perception = x }

setCha :: Integer -> Stats -> Stats
setCha x st = st { charisma = x }

setMind :: Integer -> Stats -> Stats
setMind x st = st { mindfulness = x }
