module BeginGame
    (dbBeginGame)
    where

--import Creature
import CreatureData
import CharacterData
--import Character
import DB

-- |
-- Begins the game with the specified starting player creature and the specified starting character class.
-- The character class should not be pre-applied to the creature.
--
dbBeginGame :: Creature -> CharacterClass -> DB ()
dbBeginGame _ _ = return ()