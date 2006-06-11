module CreatureData 
    (Creature(..),
    CreatureAttribute(..),
    exampleCreature1,
    maxHitPoints,
    injure,
    hitPoints,
    alive,
    dead,
    creatureEffectiveLevel,
    creatureMeleeAttackBonus,
    creatureMeleeDamageBonus,
    creatureRangedAttackBonus,
    creatureRangedDamageBonus)
    where

import StatsData
import ListUtils (count)
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
                       | Parry
                       | Dodge
			 deriving (Eq, Enum, Show, Read)

-- |
-- An example creature used for test cases.
--
exampleCreature1 = Creature 
		   { creature_stats = Stats { str=2, con=5, dex=1, int=(-2), per=4, cha=(-1), mind=(-1) },
		     creature_attribs = [Male,Toughness,Toughness,Toughness,ImprovedMeleeCombat,Parry],
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
bonusHitPoints creature = 2 * (count Toughness (creature_attribs creature))

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
-- The creature's effective level.
-- Levels are balanced against ability scores (Stats).  A +1 to
-- any ability score is considered equal in value to a single level.
-- So the first factor in computing a creature's effective level
-- is to take the sum of all of its ability scores.
-- Next add any any CreatureAttributes that count toward level adjustment.
-- Some (such as Male or Female) don't count for anything, while others may
-- count for multiple levels.
--
-- It is possible for a creature to have a negative effective level,
-- especially if its ability scores are poor.
--
creatureEffectiveLevel :: Creature -> Integer
creatureEffectiveLevel creature = let the_stats = creature_stats creature
                                      in (str the_stats) + (dex the_stats) + (con the_stats) +
                                             (int the_stats) + (per the_stats) + (cha the_stats) +
                                             (mind the_stats) +
                                             (sum $ map levelAdjustment (creature_attribs creature))

-- |
-- The amount by which a creature's effective level should be adjusted
-- based on a single occurance of the given CreatureAttribute.
--
levelAdjustment :: CreatureAttribute -> Integer

levelAdjustment Toughness = 1
levelAdjustment ImprovedMeleeCombat = 1
levelAdjustment ImprovedRangedCombat = 1
levelAdjustment Parry = 1
levelAdjustment Dodge = 1
levelAdjustment _ = 0

-- |
-- The melee attack bonus for the creature.
-- This value increases the probability that the creature will hit
-- in melee (hand-to-hand or with a wielded weapon) combat.
--
creatureMeleeAttackBonus :: Creature -> Integer
creatureMeleeAttackBonus creature = (dex $ creature_stats creature) + (bonusMeleeCombatPoints $ creature)

-- |
-- The ranged attack bonus for the creature.
-- This value increases the probability that the creature will hit
-- when shooting a ranged weapon.
--
creatureRangedAttackBonus :: Creature -> Integer
creatureRangedAttackBonus creature = (per $ creature_stats creature) + (bonusRangedCombatPoints $ creature)

-- |
-- The melee damage bonus for the creature.
-- This value is added to the damage that the creature does when it
-- strikes in melee combat.
--
creatureMeleeDamageBonus :: Creature -> Integer
creatureMeleeDamageBonus creature = (str $ creature_stats creature) + (bonusMeleeCombatPoints creature)

-- |
-- The ranged damage bonus for the creature.
-- This value is added to the damage that the creature does when it
-- strikes in ranged combat.
--
creatureRangedDamageBonus :: Creature -> Integer
creatureRangedDamageBonus creature = (bonusRangedCombatPoints creature)

-- |
-- The bonus to any action that a Creature takes in melee combat, specifically
-- attack and damage rolls.  (And to a lesser extend, armour class).
--
bonusMeleeCombatPoints :: Creature -> Integer
bonusMeleeCombatPoints creature = count ImprovedMeleeCombat (creature_attribs creature)

-- |
-- The bonux to any action that a Creature takes in ranged combat, specifically
-- attack and damage rools.  (And to a lesser extent, armour class).
--
bonusRangedCombatPoints :: Creature -> Integer
bonusRangedCombatPoints creature = count ImprovedRangedCombat (creature_attribs creature)

-- |
-- The melee armour class for the creature.  The higher a creature's armour
-- class, the lower the probability that another creature can hit it.
--
creatureMeleeArmourClass :: Creature -> Integer
creatureMeleeArmourClass creature = (dex $ creature_stats creature) + 
                                    2*(count Parry (creature_attribs creature))

-- |
-- The ranged armour class for the creature.  The higher a creature's armour
-- class, the lower the probability that another creature can hit it.
--
creatureRangedArmourClass :: Creature -> Integer
creatureRangedArmourClass creature = (per $ creature_stats creature) + 
                                     2*(count Dodge (creature_attribs creature))