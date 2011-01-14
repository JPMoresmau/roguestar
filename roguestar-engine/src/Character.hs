
module Character
    (getEligableCharacterClasses,
     getEligableBaseCharacterClasses,
     applyCharacterClass)
    where

import Alignment
import CharacterData
import CreatureAttribute
import CreatureData
import TerrainData

type Prerequisite = Creature -> Bool

data CharacterClassData = CharacterClassData {
    character_class_prerequisite :: Prerequisite,
    character_class_attributes :: CreatureAttribute }

getEligableCharacterClassesComposable :: [CharacterClass] -> Creature -> [CharacterClass]
getEligableCharacterClassesComposable allowed_classes creature =
    filter (\x -> character_class_prerequisite (classInfo x) creature || isFavoredClass x creature) allowed_classes

getEligableCharacterClasses :: Creature -> [CharacterClass]
getEligableCharacterClasses = getEligableCharacterClassesComposable all_character_classes

getEligableBaseCharacterClasses :: Creature -> [CharacterClass]
getEligableBaseCharacterClasses = getEligableCharacterClassesComposable base_character_classes

prerequisites :: [Prerequisite] -> Prerequisite
prerequisites prereqs creature = all ($ creature) prereqs

mustHave :: (CreatureScore a) => a -> Integer -> Prerequisite
mustHave score min_score creature = (rawScore score creature) >= min_score

-- |
-- Constructor function for CharacterClassData objects.
--
-- The first parameter should be the prerequisite (or more than one prerequisite using the 'prerequisites'
-- function).  The prerequisite(s) restrict what 'Creatures' can advance in the 'CharacterClass'.
--
-- The second parameter is the list of 'CreatureAttribute's that a Creature gains when it levels in the 
-- 'CharacterClass'.
--
characterClass :: Prerequisite -> CreatureAttribute -> CharacterClassData
characterClass prereqs attribs = CharacterClassData prereqs attribs

applyCharacterClass :: CharacterClass -> Creature -> Creature
applyCharacterClass character_class creature = applyToCreature (character_class & character_class_attributes (classInfo character_class)) creature

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

classInfo Barbarian = characterClass (prerequisites [mustHave Strength 15,mustHave Constitution 15]) $
		     DamageReductionTrait Melee & DamageReductionTrait Ranged & DamageReductionTrait Unarmed & ToughnessTrait & Speed & Constitution & Strength & Indifferent

classInfo Consular = characterClass (mustHave Charisma 20) $
		     Charisma & Diplomatic

classInfo Engineer = characterClass (mustHave Intellect 20) $
		     Intellect & Strategic

classInfo ForceAdept = characterClass (prerequisites [mustHave Intellect 15, mustHave Perception 15, mustHave Charisma 15, mustHave Mindfulness 15]) $
		     DefenseSkill Ranged & DefenseSkill Melee & AttackSkill Melee & Speed & Perception & Mindfulness & Indifferent

classInfo Marine = characterClass (prerequisites [mustHave Perception 15,mustHave Constitution 15]) $
		     AttackSkill Ranged & DefenseSkill Ranged & Constitution & Speed & Perception & Mindfulness & Tactical
		   
classInfo Ninja = characterClass (prerequisites [mustHave Speed 15,mustHave Perception 15]) $
		     HideSkill & DefenseSkill Melee & DefenseSkill Ranged & Speed & Indifferent

classInfo Pirate = characterClass (prerequisites [mustHave Strength 10,mustHave Perception 10, mustHave Speed 10, mustHave Charisma 10]) $
		     AttackSkill Ranged & ToughnessTrait & Strength & Speed

classInfo Scout = characterClass (prerequisites [mustHave Perception 20]) $
		     SpotSkill & Speed & Perception & Tactical

classInfo Shepherd = characterClass (prerequisites [mustHave Charisma 15,mustHave Mindfulness 15]) $
		     SpotSkill & TerrainAffinity Grass & Perception & Mindfulness & Indifferent

classInfo Thief = characterClass (mustHave Perception 20) $
		     HideSkill & Speed & Charisma & Perception & Tactical

classInfo Warrior = characterClass (prerequisites [mustHave Strength 15,mustHave Speed 15]) $
		    AttackSkill Melee & DefenseSkill Melee & Constitution & Strength & Speed & Mindfulness & Tactical

-------------------------------------------------------------------------------
--
--  Special Classes
--
--  These are special character classes that are gained by taking specific actions.
--
-------------------------------------------------------------------------------

classInfo StarChild = characterClass (prerequisites []) $
                      Intellect & Indifferent

