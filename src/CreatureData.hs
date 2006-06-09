module CreatureData 
    (Creature(..),
    CreatureAttribute(..),
    exampleCreature1,
    maxHitPoints,
    injure,
    hitPoints,
    alive,
    dead)
    where

import StatsData
import Data.List
import Ratio

data Creature = Creature { creature_stats :: Stats, 
			   creature_attribs :: [CreatureAttribute],
			   creature_name :: String,
			   creature_damage :: Integer }
		deriving (Read,Show)

--
-- A creature's attributes.
--
data CreatureAttribute = Male 
		       | Female 
		       | Neuter 
		       | Toughness
		       | ImprovedMeleeCombat
		       | ImprovedRangedCombat
		       | ImprovedGroundCombat
			 deriving (Eq, Enum, Show, Read)

-- |
-- An example creature used for test cases.
--
exampleCreature1 = Creature 
		   { creature_stats = Stats { str=2, con=5, dex=1, int=(-2), per=4, cha=(-1), mind=(-1) },
		     creature_attribs = [Male,Toughness,Toughness,Toughness],
		     creature_name = "Example-Creature-1",
		     creature_damage = 0 }

-- |
-- The maximum hit points for this Creature.  A Creature's maximum hit points are the sum of it's
-- strength, constitution, dexterity, and mindfulness, or at least 6.
--
maxHitPoints :: Creature -> Integer
maxHitPoints creature = let stats = creature_stats creature
			    in max 6 (10 + (str stats) + (con stats) + (dex stats) + (mind stats)) + bonusHitPoints creature

bonusHitPoints :: Creature -> Integer
bonusHitPoints creature = 2 * (genericLength $ filter (\x -> x == Toughness) (creature_attribs creature))

-- |
-- Does the specified damage against the Creature.
--
injure :: Integer -> Creature -> Creature
injure damage creature = let actual_damage = max 0 (damage - (con $ creature_stats creature) `quot` 2)
			     in creature { creature_damage=(creature_damage creature + actual_damage) }

-- |
-- The hit points remaining over the maximum hit points for the creature.
--
hitPoints :: Creature -> Rational
hitPoints creature = (maxHitPoints creature - creature_damage creature) % (maxHitPoints creature)

-- |
-- True if the creature is alive.
--
alive :: Creature -> Bool
alive creature = hitPoints creature >= 0

-- |
-- True if the creature is dead.
--
dead :: Creature -> Bool
dead = not . alive

-- |
-- The melee attack bonus for the creature.
--
creatureMeleeAttackBonus :: Creature -> Integer
creatureMeleeAttackBonus creature = (dex $ creature_stats creature) + (bonusMeleeCombatPoints $ creature)

-- |
-- The ranged attack bonus for the creature.
--
creatureRangedAttackBonus :: Creature -> Integer
creatureRangedAttackBonus creature = (per $ creature_stats creature) + (bonusRangedCombatPoints $ creature)

bonusMeleeCombatPoints :: Creature -> Integer
bonusMeleeCombatPoints creature = 2 * (genericLength $ filter (\x -> x == ImprovedMeleeCombat) (creature_attribs creature))

bonusRangedCombatPoints :: Creature -> Integer
bonusRangedCombatPoints creature = 2 * (genericLength $ filter (\x -> x == ImprovedRangedCombat) (creature_attribs creature))

-- |
-- The melee armour class for the creature.
--
creatureMeleeArmourClass :: Creature -> Integer
creatureMeleeArmourClass creature = (dex $ creature_stats creature)

-- |
-- The ranged armour class for the creature.
--
creatureRangedArmourClass :: Creature -> Integer
creatureRangedArmourClass creature = (per $ creature_stats creature)