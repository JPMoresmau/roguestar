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

module CreatureData 
    (Creature(..),
     CreatureGender(..),
     CreatureAttribute(..),
     applyCreatureAttribute,
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
     creatureRangedDamageBonus,
     creatureMeleeArmourClass,
     creatureRangedArmourClass,
     creatureSpeedBonus,
     creatureSize,
     creatureGender,
     characterClassLevels,
     isFavoredClass)
    where

import CharacterData
import Alignment
import StatsData
import ListUtils (count)
import Data.Maybe

data Creature = Creature { creature_stats :: Stats, 
			   creature_attribs :: [CreatureAttribute],
			   creature_name :: String,
			   creature_damage :: Integer }
		deriving (Read,Show)

data CreatureGender = Male | Female | Neuter deriving (Eq,Read,Show)

-- |
-- A creature's attributes.
--
data CreatureAttribute = Gender CreatureGender
		       | Toughness
		       | DamageReduction
		       | ImprovedMeleeCombat
		       | ImprovedRangedCombat
                       | Evasion
		       | Speed
		       | LevelPenalty
		       | LevelBonus
		       | DoesNotNegotiate
		       | DoesNotValueMoney
		       | NoKillPenalty
		       | NegotiateSkill
		       | LeadershipSkill
		       | HideSkill
		       | SpotSkill
		       | PilotSkill
		       | EngineeringSkill
		       | ScienceSkill
		       | CalmBeastAbility
		       | Regeneration
		       | ForestSurvival
		       | WaterSurvival
		       | HardStatBonus Statistic
		       | SoftStatBonus Statistic
		       | AlignmentBonus AlignmentSchool
		       | CharacterLevel CharacterClass
		       | FavoredClass CharacterClass
			 deriving (Eq, Show, Read)

-- |
-- An example creature used for test cases.
--
exampleCreature1 :: Creature
exampleCreature1 = Creature 
		   { creature_stats = Stats { str=2, con=5, dex=1, int=(-2), per=4, cha=(-1), mind=(-1) },
		     creature_attribs = [Gender Male,Toughness,Toughness,Toughness,ImprovedMeleeCombat,Evasion],
		     creature_name = "Example-Creature-1",
		     creature_damage = 0 }

-- |
-- The maximum hit points for this Creature.  A Creature's maximum hit points are the sum of it's
-- strength, constitution, dexterity, and mindfulness, or at least 6.
--
maxHitPoints :: Creature -> Integer
maxHitPoints creature = let sts = creature_stats creature
			    in max 6 (10 + (str sts) + (con sts) + (dex sts) + (mind sts)) + bonusHitPoints creature

-- |
-- The bonus hit points this creature gets for having the toughness attribute.
--
bonusHitPoints :: Creature -> Integer
bonusHitPoints creature = 2 * (count Toughness (creature_attribs creature))

-- |
-- Does the specified damage against the Creature.
--
injure :: Integer -> Creature -> Creature
injure damage creature = let actual_damage = max 0 (damage - (con $ creature_stats creature) `quot` 2)
			     in creature { creature_damage=(creature_damage creature + actual_damage) }

-- |
-- The hitPoints remaining for this creature (maxHitPoints is maximum).
--
hitPoints :: Creature -> Integer
hitPoints creature = (maxHitPoints creature - creature_damage creature)

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
--
-- This sums all of the attributes that the character has and weights them according
-- to the levelAdjustment function.  The theory is that levelAdjustment indicates the
-- power of any attribute, with a single +1 bonus to any ability score having a
-- value of 1.  (This implies that every ability score should have the same value.)
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
-- Answers the number of levels a Creature has taken in a particular CharacterClass.
-- These might not be proportional to the value of creatureEffectiveLevel, taking a level
-- in a CharacterClass sometimes increases it's effective level by more than one.
--
characterClassLevels :: CharacterClass -> Creature -> Integer
characterClassLevels character_class creature = count (CharacterLevel character_class) (creature_attribs creature)

-- |
-- The amount by which a creature's effective level should be adjusted
-- based on a single occurance of the given CreatureAttribute.
--
levelAdjustment :: CreatureAttribute -> Integer

levelAdjustment Toughness = 1
levelAdjustment ImprovedMeleeCombat = 1
levelAdjustment ImprovedRangedCombat = 1
levelAdjustment Evasion = 1
levelAdjustment LevelPenalty = 1
levelAdjustment LevelBonus = (-1)
levelAdjustment Speed = 1
levelAdjustment NoKillPenalty = 0
levelAdjustment WaterSurvival = 1
levelAdjustment ForestSurvival = 1
levelAdjustment Regeneration = 2
levelAdjustment DoesNotValueMoney = 0
levelAdjustment DoesNotNegotiate = 0
levelAdjustment (Gender {}) = 0
levelAdjustment DamageReduction = 1
levelAdjustment SoftStatBonus {} = 0
levelAdjustment HardStatBonus {} = 1
levelAdjustment AlignmentBonus {} = 0
levelAdjustment LeadershipSkill = 1
levelAdjustment NegotiateSkill = 1
levelAdjustment HideSkill = 1
levelAdjustment SpotSkill = 1
levelAdjustment PilotSkill = 1
levelAdjustment EngineeringSkill = 1
levelAdjustment ScienceSkill = 1
levelAdjustment CalmBeastAbility = 1
levelAdjustment FavoredClass {} = 0
levelAdjustment CharacterLevel {} = 0

-- |
-- Adds a CreatureAttribute to a Creature.  The CreatureAttribute stacks with or replaces any other
-- related attributes already applied to the creature, depending on the type of attribute.
-- Includes some special handling for some CreatureAttributes.
--
applyCreatureAttribute :: CreatureAttribute -> Creature -> Creature
applyCreatureAttribute (HardStatBonus statistic) = incCreatureStat statistic 
applyCreatureAttribute (SoftStatBonus statistic) = softIncCreatureStat statistic 
applyCreatureAttribute attrib = putCreatureAttribute attrib

-- |
-- applyCreatureAttribute with no special handling.
--
putCreatureAttribute :: CreatureAttribute -> Creature -> Creature
putCreatureAttribute attrib creature = creature { creature_attribs = (attrib : (creature_attribs creature))}

-- |
-- Strip all instances of a CreatureAttribute from a Creature.
--
stripCreatureAttribute :: CreatureAttribute -> Creature -> Creature
stripCreatureAttribute attrib creature = 
    creature { creature_attribs = filter (\x -> x /= attrib) $ creature_attribs creature }

incCreatureStat :: Statistic -> Creature -> Creature
incCreatureStat statistic creature = 
    let sts = creature_stats creature
	in creature { creature_stats = setStatistic statistic (succ $ getStatistic statistic sts) sts }

softIncCreatureStat :: Statistic -> Creature -> Creature
softIncCreatureStat statistic creature =
    let sts = creature_stats creature
	num = count (SoftStatBonus statistic) $ creature_attribs creature
	in if (getStatistic statistic sts < num)
	   then incCreatureStat statistic $ stripCreatureAttribute (SoftStatBonus statistic) creature
	   else putCreatureAttribute (SoftStatBonus statistic) creature

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
-- The bonus to melee attack and damage rolls.
--
bonusMeleeCombatPoints :: Creature -> Integer
bonusMeleeCombatPoints creature = count ImprovedMeleeCombat (creature_attribs creature)

-- |
-- The bonux to ranged attack and damage rolls.
--
bonusRangedCombatPoints :: Creature -> Integer
bonusRangedCombatPoints creature = count ImprovedRangedCombat (creature_attribs creature)

-- |
-- The melee armour class for the creature.  The higher a creature's armour
-- class, the lower the probability that another creature can hit it.
--
creatureMeleeArmourClass :: Creature -> Integer
creatureMeleeArmourClass creature = (dex $ creature_stats creature) + 
                                    (count Evasion (creature_attribs creature))

-- |
-- The ranged armour class for the creature.  The higher a creature's armour
-- class, the lower the probability that another creature can hit it.
--
creatureRangedArmourClass :: Creature -> Integer
creatureRangedArmourClass creature = (per $ creature_stats creature) + 
                                     (count Evasion (creature_attribs creature))

-- |
-- The number of actions per round that this creature gets.
--
creatureSpeedBonus :: Creature -> Integer
creatureSpeedBonus creature = max 1 $ (20 + (count Speed (creature_attribs creature)) - creatureSize creature)

-- |
-- The physical size of this creature, based on its attributes.
--
creatureSize :: Creature -> Integer
creatureSize creature = (con $ creature_stats creature) +
			((str $ creature_stats creature) `quot` 2) -
			((dex $ creature_stats creature) `quot` 4)

genderOf :: CreatureAttribute -> Maybe CreatureGender
genderOf attrib = case attrib of
			      Gender gender -> Just gender
			      _ -> Nothing

-- |
-- Answers the gender of this creature.
--
creatureGender :: Creature -> CreatureGender
creatureGender creature = fromMaybe Neuter $ listToMaybe $ mapMaybe genderOf $ creature_attribs creature

-- |
-- Answers true if the specified class is a favored class for this creature.
--
isFavoredClass :: CharacterClass -> Creature -> Bool
isFavoredClass character_class creature = (FavoredClass character_class) `elem` (creature_attribs creature)