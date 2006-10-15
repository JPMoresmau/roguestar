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
     creatureScore,
     Score(..),
     applyCreatureAttribute,
     exampleCreature1,
     injure,
     alive,
     dead,
     creatureGender,
     characterClassLevels,
     isFavoredClass)
    where

import CharacterData
import Alignment
import StatsData
import ListUtils (count)
import Data.Maybe
import FactionData

data Creature = Creature { creature_stats :: Stats, 
			   creature_attribs :: [CreatureAttribute],
			   creature_species_name :: String,
			   creature_random_id :: Integer, -- simply a random number attached to the creature, used by the gui to name the creature.  It is NOT required to be unique, use the toUID function in DBData for this
			   creature_damage :: Integer,
			   creature_faction :: Faction }
		deriving (Read,Show)

instance StatisticsBlock Creature where
    str creature = strength $ creature_stats creature
    dex creature = dexterity $ creature_stats creature
    con creature = constitution $ creature_stats creature
    int creature = intelligence $ creature_stats creature
    per creature = perception $ creature_stats creature
    cha creature = charisma $ creature_stats creature
    mind creature = mindfulness $ creature_stats creature

data CreatureGender = Male | Female | Neuter deriving (Eq,Read,Show)

-- |
-- A creature's attributes.
--
data CreatureAttribute = Gender CreatureGender
		       | ToughnessTrait                  -- extra hit points
		       | DamageReductionTrait            -- subtracts from any damage inflicted
		       | MeleeAttackSkill                -- increased melee accuracy
		       | MeleeDefenseSkill               -- increase melee defense
		       | PreciseStrike                   -- increase melee damage
		       | RangedAttackSkill               -- increased ranged accuracy
		       | RangedDefenseSkill              -- increase ranged defense
		       | PreciseShot                     -- increased ranged damage
		       | SpeedTrait                      -- more turns per round
		       | DoesNotNegotiate                -- AI flag -- unit does not negotiate
		       | DoesNotValueMoney               -- AI flag, unit will not take money to make up for missed negotiate check
		       | NoKillPenalty                   -- killing this unit always earns neutral experience
		       | CommandSkill                    -- skill needed to command a starship
		       | NegotiateSkill                  -- skill used to negotiate prices or peace
		       | LeadershipSkill                 -- add bonus to other unit's rolls by chatting
		       | HideSkill                       -- unit is harder to see
		       | SpotSkill                       -- unit can see farther away
		       | PilotSkill                      -- skill needed to operate single-pilot ships
		       | EngineeringSkill                -- skill used to build and disassemble devices
		       | RepairSkill                     -- skill used to repair starships
		       | ScienceSkill                    -- equivalent of spot for starships
		       | CalmBeastAbility                -- equivalent of negotiate for non-sentient creatures
		       | RegenerationAbility             -- special ability to recharge hit points using psi points
		       | ForestSurvivalSkill             -- hide, speed, and defense in forest
		       | WaterSurvivalSkill              -- hide, speed, and defense in water, plus resistance to drowning
		       | HardStatBonus Statistic         -- when applied, instantly raises statistic by 1
		       | SoftStatBonus Statistic         -- when applied multiple times, instantly raises statistic as a progression
		       | AlignmentBonus AlignmentSchool  -- represents the creature's tendency toward strategic, tactical, diplomatic, or indifferent thinking styles
		       | CharacterLevel CharacterClass   -- record of a character class being applied to the creature, has no game effect
		       | FavoredClass CharacterClass     -- creature is able to take the specified class without any prerequisites
			 deriving (Eq, Show, Read)

data Score = MaxHitPoints
	   | HitPoints
	   | DamageReduction
	   | MeleeAttack
	   | MeleeDefense
	   | MeleeDamage
	   | RangedAttack
	   | RangedDefense
	   | RangedDamage
	   | Speed
	   | EffectiveLevel
	   | Spot
	   | Hide

-- |
-- An example creature used for test cases.
--
exampleCreature1 :: Creature
exampleCreature1 = Creature 
		   { creature_stats = Stats { strength=2, constitution=5, dexterity=1, intelligence=(-2), perception=4, charisma=(-1), mindfulness=(-1) },
		     creature_attribs = [Gender Male,
					 ToughnessTrait,
					 ToughnessTrait,
					 ToughnessTrait,
					 MeleeAttackSkill,
					 MeleeDefenseSkill,
					 RangedDefenseSkill],
		     creature_species_name = "Example-Creature-1",
		     creature_random_id=0,
		     creature_damage = 0,
		     creature_faction = MonstersInc }

creatureScore :: Score -> Creature -> Integer
creatureScore MaxHitPoints = \c -> max 6 (20 + (str c) + (con c) + (dex c) + (mind c)) + 2 * attributeCount ToughnessTrait c
creatureScore HitPoints = \c -> creatureScore MaxHitPoints c - creature_damage c
creatureScore DamageReduction = statPlusDouble Constitution DamageReductionTrait
creatureScore MeleeAttack = statPlusDouble Dexterity MeleeAttackSkill
creatureScore MeleeDefense = statPlusDouble Dexterity MeleeDefenseSkill
creatureScore MeleeDamage = statPlusDouble Strength PreciseStrike
creatureScore RangedAttack = statPlusDouble Perception RangedAttackSkill
creatureScore RangedDefense = statPlusDouble Perception RangedDefenseSkill 
creatureScore RangedDamage = \c -> max 0 $ per c + attributeCount PreciseShot c
creatureScore Speed = \c -> 20 + attributeCount SpeedTrait c
creatureScore Spot = statPlusDouble Perception SpotSkill
creatureScore Hide = statPlusDouble Perception HideSkill

-- |
-- The creature's effective level.
--
-- This sums all of the ability scores and attributes that a creature has and determines
-- approximately how powerful the creature is.
--
-- It is possible for a creature to have a negative effective level,
-- especially if its ability scores are poor.
--
creatureScore EffectiveLevel = \c -> sum (map ($ c) [str,dex,con,int,per,cha,mind] ++
					  map levelAdjustment (creature_attribs c))

attributeCount :: CreatureAttribute -> Creature -> Integer
attributeCount attrib creature = count attrib $ creature_attribs creature

-- |
-- The standard way to calculate any score is to add the relevant Statistic to twice the number of
-- ranks in the relevant skill.
--
statPlusDouble :: Statistic -> CreatureAttribute -> Creature -> Integer
statPlusDouble statistic attrib creature = max 0 $ 20 + getStatistic statistic creature + 2 * attributeCount attrib creature

-- |
-- Does the specified damage against the Creature.
--
injure :: Integer -> Creature -> Creature
injure damage creature = let actual_damage = max 0 (damage - (con $ creature_stats creature) `quot` 2)
			     in creature { creature_damage=(creature_damage creature + actual_damage) }

-- |
-- True if the creature is alive.
--
alive :: Creature -> Bool
alive creature = creatureScore HitPoints creature >= 0

-- |
-- True if the creature is dead.
--
dead :: Creature -> Bool
dead = not . alive

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

levelAdjustment ToughnessTrait = 1
levelAdjustment MeleeAttackSkill = 1
levelAdjustment MeleeDefenseSkill = 1
levelAdjustment PreciseStrike = 1
levelAdjustment RangedAttackSkill = 1
levelAdjustment RangedDefenseSkill = 1
levelAdjustment PreciseShot = 1
levelAdjustment SpeedTrait = 2
levelAdjustment NoKillPenalty = 0
levelAdjustment WaterSurvivalSkill = 1
levelAdjustment ForestSurvivalSkill = 1
levelAdjustment RegenerationAbility = 2
levelAdjustment DoesNotValueMoney = 0
levelAdjustment DoesNotNegotiate = 0
levelAdjustment (Gender {}) = 0
levelAdjustment DamageReductionTrait = 1
levelAdjustment SoftStatBonus {} = 0
levelAdjustment HardStatBonus {} = 1
levelAdjustment AlignmentBonus {} = 0
levelAdjustment LeadershipSkill = 1
levelAdjustment NegotiateSkill = 1
levelAdjustment CommandSkill = 1
levelAdjustment HideSkill = 1
levelAdjustment SpotSkill = 1
levelAdjustment PilotSkill = 1
levelAdjustment EngineeringSkill = 1
levelAdjustment RepairSkill = 1
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