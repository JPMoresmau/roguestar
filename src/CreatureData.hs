
module CreatureData 
    (Creature(..),
     CreatureGender(..),
     CreatureAttribute(..),
     creatureScore,
     Score(..),
     applyCreatureAttribute,
     exampleCreature1,
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
			   creature_random_id :: Integer, -- random number attached to the creature, not unique
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
		       | RangedAttackSkill               -- increased ranged accuracy
		       | RangedDefenseSkill              -- increase ranged defense
		       | SpeedTrait                      -- more turns per round
		       | HideSkill                       -- unit is harder to see
		       | SpotSkill                       -- unit can see farther away
                       | JumpSkill                       -- unit can jump/teleport short distances
                       | StatBonus Statistic             -- +1 to any statistic
                       | AlignmentBonus EthicalAlignment -- represents the creature's tendency toward strategic, tactical, diplomatic, or indifferent thinking styles
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
	   | Speed Statistic
	   | EffectiveLevel
	   | Spot
	   | Hide
           | Jump

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
		     creature_faction = Monsters }

creatureScore :: Score -> Creature -> Integer
creatureScore MaxHitPoints = \c -> max 6 (str c + con c + dex c + mind c) + 2 * attributeCount ToughnessTrait c
creatureScore HitPoints = \c -> creatureScore MaxHitPoints c - creature_damage c
creatureScore DamageReduction = statPlusDouble Constitution DamageReductionTrait
creatureScore MeleeAttack = statPlusDouble Dexterity MeleeAttackSkill
creatureScore MeleeDefense = statPlusDouble Dexterity MeleeDefenseSkill
creatureScore MeleeDamage = getStatistic Strength
creatureScore RangedAttack = statPlusDouble Dexterity RangedAttackSkill
creatureScore RangedDefense = statPlusDouble Perception RangedDefenseSkill 
creatureScore (Speed by_statistic) = \c -> max 1 $ getStatistic by_statistic c + attributeCount SpeedTrait c
creatureScore Spot = statPlusDouble Perception SpotSkill
creatureScore Hide = \c -> max 0 $ per c + attributeCount HideSkill c
creatureScore Jump = statPlusDouble Strength JumpSkill

-- |
-- The creature's effective level.
--
-- This sums all of the ability scores and attributes that a creature has and determines
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
statPlusDouble statistic attrib creature = max 0 $ getStatistic statistic creature + 2 * attributeCount attrib creature

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
levelAdjustment RangedAttackSkill = 1
levelAdjustment RangedDefenseSkill = 1
levelAdjustment SpeedTrait = 2
levelAdjustment (StatBonus _) = 1
levelAdjustment (Gender {}) = 0
levelAdjustment DamageReductionTrait = 1
levelAdjustment AlignmentBonus {} = 0
levelAdjustment HideSkill = 1
levelAdjustment SpotSkill = 1
levelAdjustment FavoredClass {} = 0
levelAdjustment CharacterLevel {} = 0
levelAdjustment JumpSkill = 1

-- |
-- Adds a CreatureAttribute to a Creature.  The CreatureAttribute stacks with or replaces any other
-- related attributes already applied to the creature, depending on the type of attribute.
-- Includes some special handling for some CreatureAttributes.
--
applyCreatureAttribute :: CreatureAttribute -> Creature -> Creature
applyCreatureAttribute (StatBonus statistic) = incCreatureStat statistic 
applyCreatureAttribute attrib = putCreatureAttribute attrib

-- |
-- applyCreatureAttribute with no special handling.
--
putCreatureAttribute :: CreatureAttribute -> Creature -> Creature
putCreatureAttribute attrib creature = creature { creature_attribs = (attrib : (creature_attribs creature))}

incCreatureStat :: Statistic -> Creature -> Creature
incCreatureStat statistic creature = 
    let sts = creature_stats creature
	in creature { creature_stats = setStatistic statistic (succ $ getStatistic statistic sts) sts }

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
