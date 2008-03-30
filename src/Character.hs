
module Character
    (getEligableCharacterClasses,
     getEligableBaseCharacterClasses,
     applyCharacterClass)
    where

import Data.List as List
import Alignment
import CharacterData
import CreatureData
import StatsData

type Prerequisite = Creature -> Bool

type CharacterClassData = (Prerequisite,[CreatureAttribute])

getEligableCharacterClassesComposable :: [CharacterClass] -> Creature -> [CharacterClass]
getEligableCharacterClassesComposable allowed_classes creature = 
    filter (\x -> (fst $ classInfo x) creature) allowed_classes

getEligableCharacterClasses :: Creature -> [CharacterClass]
getEligableCharacterClasses = getEligableCharacterClassesComposable all_character_classes

getEligableBaseCharacterClasses :: Creature -> [CharacterClass]
getEligableBaseCharacterClasses = getEligableCharacterClassesComposable base_character_classes

prerequisites :: [Prerequisite] -> Prerequisite
prerequisites prereqs creature = all ($ creature) prereqs

mustHave :: Statistic -> Integer -> Prerequisite
mustHave statistic min_score creature = (getStatistic statistic $ creature_stats creature) >= min_score

-- |
-- Constructor function for CharacterClassData objects.
--
-- First parameter should be the CharacterClass.
--
-- The second parameter should be the prerequisite (or more than one prerequisite using the prerequisites 
-- function).  The prerequisite(s) restrict what Creatures can advance in the CharacterClass.
--
-- The third parameter is the list CreatureAttributes that a Creature gains when it levels in the 
-- CharacterClass.  
--
characterClass :: CharacterClass -> Prerequisite -> [CreatureAttribute] -> CharacterClassData
characterClass character_class prereqs level_xforms = 
    ((\x -> prereqs x || isFavoredClass character_class x),CharacterLevel character_class : level_xforms)

applyCharacterClass :: CharacterClass -> Creature -> Creature
applyCharacterClass character_class creature =
    if (fst $ classInfo character_class) creature
       then foldr applyCreatureAttribute creature (snd $ classInfo character_class)
       else error "tried to applyCharacterClass with a creature that didn't meet prerequisites"

classInfo :: CharacterClass -> CharacterClassData

-------------------------------------------------------------------------------
--
--  Base Classes
-- 
--  These are base classes: these classes have very low prerequisites,
--  with the intention that characters can choose them at the beginning
--  of a game.  They also contain extra information about the character's
--  starting equipment and situation.
--
-------------------------------------------------------------------------------

classInfo Barbarian = characterClass Barbarian (prerequisites [mustHave Strength 15,mustHave Constitution 15])
		      [ToughnessTrait,DamageReductionTrait,SpeedTrait,StatBonus Constitution,StatBonus Strength,AlignmentBonus Indifferent]

classInfo Consular = characterClass Consular (mustHave Charisma 20)
		     [StatBonus Charisma,AlignmentBonus Diplomatic]

classInfo Engineer = characterClass Engineer (mustHave Intelligence 20)
		     [StatBonus Intelligence,AlignmentBonus Strategic]

classInfo ForceAdept = characterClass ForceAdept (prerequisites [mustHave Intelligence 15, mustHave Perception 15, mustHave Charisma 15, mustHave Mindfulness 15])
		       [RangedDefenseSkill,MeleeDefenseSkill,MeleeAttackSkill,StatBonus Perception,StatBonus Mindfulness,AlignmentBonus Indifferent]

classInfo Marine = characterClass Marine (prerequisites [mustHave Perception 15,mustHave Constitution 15])
		   [RangedAttackSkill,
		    RangedDefenseSkill,
		    StatBonus Constitution,
		    StatBonus Dexterity,
		    StatBonus Perception,
		    StatBonus Mindfulness,
		    AlignmentBonus Tactical]
		   
classInfo Ninja = characterClass Ninja (prerequisites [mustHave Dexterity 15,mustHave Perception 15])
		  [HideSkill,MeleeDefenseSkill,RangedDefenseSkill,StatBonus Dexterity,AlignmentBonus Indifferent]

classInfo Pirate = characterClass Pirate (prerequisites [mustHave Strength 10,mustHave Perception 10, mustHave Dexterity 10, mustHave Charisma 10])
		   [RangedAttackSkill,ToughnessTrait,StatBonus Strength,StatBonus Dexterity]

classInfo Scout = characterClass Scout (prerequisites [mustHave Perception 20])
		  [SpotSkill,StatBonus Dexterity,StatBonus Perception,AlignmentBonus Tactical]

classInfo Shepherd = characterClass Shepherd (prerequisites [mustHave Charisma 15,mustHave Mindfulness 15])
		     [SpotSkill,StatBonus Perception,StatBonus Mindfulness,AlignmentBonus Indifferent]

classInfo Thief = characterClass Thief (mustHave Perception 20)
		  [HideSkill,StatBonus Dexterity,StatBonus Charisma,StatBonus Perception,AlignmentBonus Tactical]

classInfo Warrior = characterClass Warrior (prerequisites [mustHave Strength 15,mustHave Dexterity 15])
		    [MeleeAttackSkill,
	             MeleeDefenseSkill,
		     StatBonus Constitution,
		     StatBonus Strength,
		     StatBonus Dexterity,
		     StatBonus Mindfulness,
		     AlignmentBonus Tactical]

