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

module Races
    (selectPlayerRace,
     player_race_names,
     all_races,
     allowed_player_races,
     anachronid,
     male_anachronid,
     female_anachronid,
     androsynth,
     ascendant,
     caduceator,
     encephalon,
     kraken,
     goliath,
     hellion,
     myrmidon,
     perennial,
     recreant,
     reptilian)
    where

import Data.Char
import StatsData
import CreatureData
import CharacterData
import SpeciesData
import AttributeData
import Data.List

all_races :: [Species]
all_races = [anachronid,
	     androsynth,
	     ascendant,
	     caduceator,
	     encephalon,
	     goliath,
	     hellion,
	     kraken,
	     myrmidon,
	     perennial,
	     recreant,
	     reptilian]

allowed_player_races :: [Species]
allowed_player_races = [female_anachronid,
			androsynth,
			ascendant,
			caduceator,
			encephalon,
			goliath,
			hellion,
			kraken,
			myrmidon,
			perennial,
			recreant,
			reptilian]

player_race_names :: [String]
player_race_names = map (map toLower . species_name) allowed_player_races

selectPlayerRace :: String -> Maybe Species
selectPlayerRace race_name = find 
			     (\x -> (map toLower $ species_name x) == map toLower race_name) 
			     allowed_player_races

-- |
-- Six-legged species that move through time unusually slowly, making them appear (to outsiders),
-- to move very quickly.  Yes, they eat their own males -- squad leaders are always female.
-- Anachronids in modern times are often seen working as mercenaries and scouts for the Imperial Alliance,
-- although as a species they are scattered on many worlds -- their homeworld having been destroyed
-- in war with the myrmidons many centuries past.
--
-- Homeworld: Anachrona Prime
-- Second Homeworld: Anachrona Inferior
--
anachronid :: Species
anachronid = Species {
		      averages = Stats { strength=1, dexterity=1, constitution=(-2), intelligence=(-2), perception=0, charisma=(-2), mindfulness=0 },
		      distributions = (stats 3),
		      attribute_generator = ([female 0.05,
					      AttributeAlways $ FavoredClass Barbarian] ++ 
		                             (multipleAttribute SpeedTrait (3,5))),
		      species_name = "anachronid"
		     }

female_anachronid :: Species
female_anachronid = anachronid { attribute_generator = [female 1] ++ (attribute_generator anachronid) }

male_anachronid :: Species
male_anachronid = anachronid { attribute_generator = [male 1,AttributeAlways NoKillPenalty] ++ (attribute_generator anachronid) }

-- |
-- Androsynths are androids, created by the Ascendants to be their physical bodies before
-- they learned to transform themselves into pure psionic energy.  The Androsynths were left
-- behind with all of the memories but none of the emotions of their creators.  Over hundreds of
-- years they developed their own civilization and culture.  They have few emotions other their
-- ongoing dedication to the ideals of their ancestors.
--
-- Homeworld: Synthetica Zero.
--
androsynth :: Species
androsynth = Species {
		      averages = (stats (-1)) { intelligence=7, charisma=(-2) },
		      distributions = (stats 0) { intelligence=0 },
		      attribute_generator = ([AttributeAlways DoesNotValueMoney,
					      AttributeAlways $ FavoredClass Engineer] ++
					     (multipleAttribute DamageReductionTrait (3,3))), --also: some resistance to kinetic energy
		      species_name = "androsynth"
		     }

-- |
-- This ancient race (who early in their evolution had the form of flightless birds) was known for its
-- craft in the force and psionic arts.  Ascendant force knights once guaranteed peace in the galaxy.
-- As they evolved, their bodies were no longer able to contain their powerful psionic energies,
-- and they became pure psionic life forces.  It is rumored that the energy beings recognized as the
-- Ascendants are actually mere shadows of what have grown into vastly powerful, almost godlike creatures
-- engaged in an epic battle against evil in a dimension beyond mortal comprehension.  At least, that
-- theory tries to explain why they no longer work to maintain peace in the galaxy of today.
--
-- The last of the Ascendant knights still posessing a physical form signed with the Interstellar Concordance,
-- but its not clear if the Ascendants still recognize that alliance.
--
ascendant :: Species
ascendant = Species {
		     averages = Stats { strength=(-4), dexterity=(-1), constitution=(-1), intelligence=2, perception=(-1), charisma=1, mindfulness=4 },
		     distributions = (stats 4),
		     attribute_generator = [AttributeAlways $ FavoredClass Shepherd,
					    male 0.45], -- also: very high resistance to kinetic,fire,cold
		     species_name = "ascendant"
		    }

-- |
-- This serpentine species has a unique facility with language, and in the last thousand years
-- have supersceded the Ascendants as peacemakers in the galaxy.  They are the founders of the
-- Interstellar Concordance, but they have seen their influence wane in the face of the reptilians
-- and kraken, who know how to leverage business relationships to faciliatate change.
--
-- Homeworld: Caducea Prime
--
caduceator :: Species
caduceator = Species {
		      averages = Stats { strength=(-1), dexterity=2, constitution=(-1), intelligence=(-2), perception=(-2), charisma=2, mindfulness=2 },
		      distributions = (stats 5),
		      attribute_generator = [male 0.6,
					     AttributeAlways $ FavoredClass Consular,
					     AttributeAlways DoesNotValueMoney], -- also: vulnerability to heat and cold
		      species_name = "caduceator"
		     }
-- |
-- Despite their name, these massive creatures are made mostly from fat, not brain matter.
-- They are the most intelligent life forms in the universe, but their lack of mobility
-- limits their experience of life, which in turn has limited their influence.  Their homeworld
-- is a member planet of the Imperial Alliance, for whom they develop powerful computers.
--
encephalon :: Species
encephalon = Species {
		      averages = Stats { strength=(-10), dexterity=(-10), constitution=20, intelligence=20, perception=(-10), charisma=(-5), mindfulness=(-5) },
		      distributions = (stats 5),
		      attribute_generator = [male 0.95,
					     AttributeAlways $ FavoredClass Engineer,
					     AttributeAlways DoesNotValueMoney],
		      species_name = "encephalon"
		     }
		      
		      

-- |
-- Small humanoids with four hands, often recognized as skilled surgeons and engineers.
-- The Hellion homeworld is a member of the Interstellar Concordance.
--
-- Homeworld: Hellios
--
hellion :: Species
hellion = Species {
		   averages = Stats { strength=(-1), dexterity=1, constitution=(-1), intelligence=1, perception=2, charisma=(-1), mindfulness=(-1) },
		   distributions = (stats 10),
		   attribute_generator = [AttributeAlways $ FavoredClass Scout,
					  AttributeAlways $ FavoredClass Marine,
					  AttributeAlways $ FavoredClass Thief,
					  male 0.65],
		   species_name = "hellion"
		  }

-- |
-- Large, tough, gray aliens with big heads and big eyes that like to smash.
--
-- Homeworld: Golia-Indri
--
goliath :: Species
goliath = Species {
		   averages = Stats { strength=3, dexterity=(-1), constitution=4, intelligence=(-2), perception=0, charisma=(-4), mindfulness=(-3) },
		   distributions = (stats 4),
		   attribute_generator = ([male 0.55,
					   AttributeAlways $ FavoredClass Barbarian,
					   AttributeAlways $ FavoredClass Warrior,
					   AttributeAlways $ FavoredClass Scout] ++
					  (multipleAttribute ToughnessTrait (3,7))),
		   species_name = "goliath"
		  }

-- |
-- Aquatic species with tenticles.  The kraken homeworld is the capital of the Imperial Aliance.
--
-- Homeworld: Quatica
--
kraken :: Species
kraken = Species {
		  averages = Stats { strength=2, dexterity=2, constitution=4, intelligence=0, perception=(-2), charisma=4, mindfulness=0 },
		  distributions = (stats 2) { mindfulness=6 },
		  attribute_generator = ([male 0.5,
					  AttributeAlways $ FavoredClass Privateer,
					  AttributeAlways WaterSurvivalSkill]),
		  species_name = "kraken"
		 }

-- |
-- Ant-like species.  An inventive species that effectively uses consensus decision making.  They are
-- signatories to the Pan Galactic Treaty Organization even though they have no formal government. 
-- The Myrmidon homeworld houses the Headquarters of the Pan Galactic Treaty Organization, but was devestated
-- in the attack that begins the game.
--
-- Homeworld: Myrmidon Prime
--
myrmidon :: Species
myrmidon = Species {
		    averages = Stats { strength=2, dexterity=(-1), constitution=(-1), intelligence=4, perception=(-2), charisma=(-2), mindfulness=0 },
		    distributions = (stats 4),
		    attribute_generator = [AttributeAlways $ FavoredClass Barbarian,
					   AttributeAlways $ FavoredClass Engineer,
					   AttributeAlways $ FavoredClass Warrior,
					   female 1],
		    species_name = "myrmidon"
		   }

-- |
-- Plant creatures!  Mobile flowering shrubs.  Although their homeword has been a member of the Pan Galactic
-- Treaty Organization since shortly after it was first established, they have never participated in any
-- actions with that organization, including the action to defend the Myrmidon homeworld.
--
-- Homeworld: Terrene
--
perennial :: Species
perennial = Species {
		     averages = Stats { strength=(-4), dexterity=(-4), constitution=1, intelligence=1, perception=(-1), charisma=2, mindfulness=2 },
		     distributions = (stats 5),
		     attribute_generator = ([AttributeAlways $ FavoredClass Barbarian,
					     AttributeAlways $ FavoredClass Consular,
					     AttributeAlways $ FavoredClass Shepherd,
					     AttributeAlways DamageReductionTrait,
					     AttributeAlways ForestSurvivalSkill,
					     AttributeAlways WaterSurvivalSkill,
					     AttributeAlways RegenerationAbility,
					     AttributeAlways DoesNotValueMoney]), -- also: resistance to cold and fire
		     species_name = "perennial"
		   }

-- |
-- Recreants are not a single species, but a variety of different self-replicating machines left over from
-- the Myrmidon-Anachronid war.  Most now dwell on the former Anachronid homeworld, but they know how to
-- operate the starships that the Anachronids left on that planet, and sometimes travel to other worlds
-- where, if antagonized, they spread various forms of horrifying destruction.
--
-- Homeworld: Anachrona Prime
--
recreant :: Species
recreant = Species {
		    averages = (stats (-4)) { dexterity=4 },
		    distributions = (stats 3),
		    attribute_generator = ((multipleAttribute RegenerationAbility (15,25)) ++
					   [AttributeAlways DoesNotValueMoney,
					    AttributeAlways NoKillPenalty,
					    AttributeAlways $ FavoredClass Barbarian]), -- also: resistance to every energy type escept kinetic and built-in plasma weapons
		    species_name = "recreant"
		   }

-- |
-- An adaptable, velociraptor-esque species built for combat but often appearing as skilled negotiators.
-- The reptilian homeworld is a signatory planet to the Pan Galactic Treaty Organization.
--
reptilian :: Species
reptilian = Species {
		     averages = Stats { strength=1, dexterity=1, constitution=1, intelligence=(-4), perception=0, charisma=2, mindfulness=(-4) },
		     distributions = (stats 3),
		     attribute_generator = ([male 0.35,
					     AttributeAlways $ FavoredClass Warrior,
					     AttributeAlways $ FavoredClass Consular] ++
					    (multipleAttribute ToughnessTrait (2,3)) ++
					    (multipleAttribute SpeedTrait (0,2)) ++
					    (multipleAttribute MeleeAttackSkill (2,5))), -- also: vulnerability to cold and fire
		     species_name = "reptilian"
		    }