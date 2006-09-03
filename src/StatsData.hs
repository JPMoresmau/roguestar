--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

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