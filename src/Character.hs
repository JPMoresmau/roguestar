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
import ListUtils

type Prerequisite = Creature -> Bool

type CharacterClassData = (Prerequisite,[[CreatureAttribute]])

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
-- CharacterClass.  The CreatureAttributes are listed in order of descending frequency, as defined by the 
-- listByFrequency function.  Thus, CreatureAttributes that are listed like this: 
-- [[Toughness,HardStatBonus Constitution],
--  [ImprovedMeleeCombat,HardStatBonus Strength],
--  [ImprovedRangedCombat,HardStatBonus Perception]]
-- Yield a cumulative class table that looks something like this:
-- 1: Toughness*1, con+1
-- 2: Toughness*1, Melee Combat*1, con+1, str+1
-- 3: Toughness*2, Melee Combat*1, con+2, str+1
-- 4: Toughness*2, Melee Combat*1, Ranged Combat*1, con+2, str+1, per+1
-- 5: Toughness*3, Melee Combat*1, Ranged Combat*1, con+3, str+1, per+1
-- 6: Toughness*3, Melee Combat*2, Ranged Combat*1, con+3, str+2, per+1
-- 7: Toughness*4, Melee Combat*2, Ranged Combat*1, con+4, str+2, per+1
-- 8: Toughness*4, Melee Combat*2, Ranged Combat*2, con+4, str+2, per+2
-- And so forth.
--
characterClass :: CharacterClass -> Prerequisite -> [[CreatureAttribute]] -> CharacterClassData
characterClass character_class prereqs level_xforms = 
    ((\x -> prereqs x || isFavoredClass character_class x),map ((CharacterLevel character_class) :) $ listByFrequency level_xforms)

applyCharacterClass :: CharacterClass -> Creature -> Creature
applyCharacterClass character_class creature =
    if (fst $ classInfo character_class) creature
       then foldr applyCreatureAttribute creature $ 
	    genericIndex (snd $ classInfo character_class) (characterClassLevels character_class creature)
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

classInfo Barbarian = characterClass Barbarian (prerequisites [mustHave Strength 1,mustHave Constitution 1])
		      [[Toughness,HardStatBonus Constitution,HardStatBonus Strength,AlignmentBonus Indifferent]]

classInfo Consular = characterClass Consular (mustHave Charisma 1)
		     [[NegotiateSkill,SoftStatBonus Charisma,AlignmentBonus Diplomatic],
		      [LeadershipSkill,SoftStatBonus Charisma,AlignmentBonus Diplomatic]]

classInfo Engineer = characterClass Engineer (mustHave Intelligence 1)
		     [[EngineeringSkill,SoftStatBonus Intelligence,AlignmentBonus Strategic],
		      [EngineeringSkill,AlignmentBonus Strategic],
		      [ScienceSkill,SoftStatBonus Intelligence,AlignmentBonus Strategic]]

classInfo ForceAdept = characterClass ForceAdept (prerequisites [mustHave Intelligence 1, mustHave Perception 1, mustHave Charisma 2, mustHave Mindfulness 4])
		       [[Evasion,HardStatBonus Mindfulness,AlignmentBonus Indifferent],
			[NegotiateSkill,HardStatBonus Charisma,AlignmentBonus Diplomatic],
			[ImprovedMeleeCombat,HardStatBonus Dexterity,AlignmentBonus Tactical],
			[PilotSkill,HardStatBonus Intelligence,AlignmentBonus Strategic],
			[HideSkill,HardStatBonus Perception,AlignmentBonus Tactical],
			[SpotSkill,HardStatBonus Perception,AlignmentBonus Tactical]]

classInfo Marine = characterClass Marine (mustHave Perception 0)
		   [[ImprovedRangedCombat,SoftStatBonus Constitution,SoftStatBonus Dexterity,SoftStatBonus Perception,SoftStatBonus Mindfulness,AlignmentBonus Tactical]]
		   
classInfo Ninja = characterClass Ninja (prerequisites [mustHave Dexterity 1,mustHave Perception 1])
		  [[Evasion,AlignmentBonus Indifferent],
		   [HardStatBonus Dexterity,AlignmentBonus Indifferent],
		   [HardStatBonus Perception,AlignmentBonus Indifferent]]

classInfo Pilot = characterClass Pilot (prerequisites [mustHave Intelligence 2,mustHave Perception 2])
		  [[PilotSkill,SoftStatBonus Intelligence,SoftStatBonus Mindfulness,SoftStatBonus Perception,AlignmentBonus Tactical],
		   [PilotSkill,SoftStatBonus Intelligence,SoftStatBonus Mindfulness,SoftStatBonus Perception,AlignmentBonus Tactical],
		   [EngineeringSkill,SoftStatBonus Intelligence,AlignmentBonus Strategic],
		   [ScienceSkill,SoftStatBonus Intelligence,AlignmentBonus Strategic]]

classInfo Privateer = characterClass Privateer (prerequisites [mustHave Intelligence 0, mustHave Perception 0, mustHave Charisma 0, mustHave Mindfulness 0])
		      [[ImprovedRangedCombat,SoftStatBonus Dexterity,AlignmentBonus Diplomatic],
		       [PilotSkill,SoftStatBonus Perception,AlignmentBonus Tactical],
		       [NegotiateSkill,SoftStatBonus Charisma,AlignmentBonus Diplomatic],
		       [Toughness,SoftStatBonus Strength,AlignmentBonus Tactical],
		       [LeadershipSkill,SoftStatBonus Charisma,AlignmentBonus Diplomatic]]

classInfo Scout = characterClass Scout (prerequisites [mustHave Perception 0])
		  [[SpotSkill,SoftStatBonus Dexterity,SoftStatBonus Perception,AlignmentBonus Tactical],
		   [ScienceSkill,SoftStatBonus Intelligence,SoftStatBonus Mindfulness,AlignmentBonus Strategic]]

classInfo Shepherd = characterClass Shepherd (prerequisites [mustHave Charisma 0,mustHave Mindfulness 0])
		     [[CalmBeastAbility,SoftStatBonus Charisma,SoftStatBonus Mindfulness,AlignmentBonus Indifferent],
		      [ScienceSkill,SoftStatBonus Charisma,AlignmentBonus Strategic],
		      [SpotSkill,SoftStatBonus Perception,SoftStatBonus Mindfulness,AlignmentBonus Tactical],
		      [NegotiateSkill,SoftStatBonus Charisma,AlignmentBonus Diplomatic]]

classInfo Thief = characterClass Thief (mustHave Perception 1)
		  [[HideSkill,SoftStatBonus Dexterity,SoftStatBonus Charisma,SoftStatBonus Mindfulness,AlignmentBonus Tactical]]

classInfo Warrior = characterClass Warrior (prerequisites [mustHave Strength 1,mustHave Dexterity 1])
		    [[ImprovedMeleeCombat,SoftStatBonus Constitution,SoftStatBonus Strength,SoftStatBonus Dexterity,SoftStatBonus Mindfulness,AlignmentBonus Tactical]]

-------------------------------------------------------------------------------
--
--  Expert Classes
-- 
--  These classes are extensions of the base classes.  They have moderate
--  prerequisites and improve on skills that the base classes provide.
--
-------------------------------------------------------------------------------

-- Additional notes on Advanced Character Classes

-- Barbarian -> Berzerker (ability to melee attack all adjacent squares)

-- Consular (negotiate+leadership) -> Arbiter (command+negotiate), Commodore (command+leadership)

-- Shepherd -> BeastMaster (beast calming+barbarian), Druid (beast calming,negotiate,weather controll)

-- Druid+Arbiter/Commodore -> Archon (command,weather control,lightning attack)

-- ForceAdept -> ForceApprentice (force push ability) -> ForceKnight (omnidirectional-force-push ability) -> ForceMaster (lightning attack)