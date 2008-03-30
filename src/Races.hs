
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
anachronid :: Species
anachronid = Species {
		      averages = Stats { strength=10, dexterity=10, constitution=9, intelligence=8, perception=10, charisma=8, mindfulness=7 },
		      distributions = (stats 13),
		      attribute_generator = ([female 0.05,
					      AttributeAlways $ FavoredClass Barbarian,
					      AttributeAlways $ FavoredClass Pirate] ++ 
		                             (multipleAttribute SpeedTrait (3,5))),
		      species_name = "anachronid"
		     }

female_anachronid :: Species
female_anachronid = anachronid { attribute_generator = [female 1] ++ (attribute_generator anachronid) }

male_anachronid :: Species
male_anachronid = anachronid { attribute_generator = [male 1] ++ (attribute_generator anachronid) }

-- |
-- Androsynths are androids, created by the Ascendants to be their physical bodies before
-- they learned to transform themselves into pure psionic energy.  The Androsynths were left
-- behind with all of the memories but none of the emotions of their creators.  Over hundreds of
-- years they developed their own civilization and culture.  They have few emotions other their
-- ongoing dedication to the ideals of their ancestors.
--
androsynth :: Species
androsynth = Species {
		      averages = (stats (14)) { intelligence=22, charisma=8 },
		      distributions = (stats 0) { intelligence=0 },
		      attribute_generator = ([AttributeAlways $ FavoredClass Engineer] ++
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
		     averages = Stats { strength=6, dexterity=9, constitution=9, intelligence=12, perception=9, charisma=11, mindfulness=20 },
		     distributions = (stats 14) { mindfulness=20 },
		     attribute_generator = [AttributeAlways $ FavoredClass Shepherd,
		                            AttributeAlways $ FavoredClass ForceAdept,
					    male 0.45], -- also: very high resistance to kinetic,fire,cold
		     species_name = "ascendant"
		    }

-- |
-- This serpentine species has a unique facility with language, and in the last thousand years
-- have supersceded the Ascendants as peacemakers in the galaxy.  They are the founders of the
-- Interstellar Concordance, but they have seen their influence wane in the face of the reptilians
-- and kraken, who know how to leverage business relationships to faciliatate their political will.
--
caduceator :: Species
caduceator = Species {
		      averages = Stats { strength=9, dexterity=12, constitution=9, intelligence=8, perception=8, charisma=16, mindfulness=12 },
		      distributions = (stats 15),
		      attribute_generator = [male 0.6,
					     AttributeAlways $ FavoredClass Consular], -- also: vulnerability to heat and cold
		      species_name = "caduceator"
		     }
-- |
-- Encephalons are a sort of hyper-intelligent fungus, in fact, they are considered the most intelligent 
-- life forms in the galaxy, but their mobility and alertness are limited, dependant as their are on their various machine servants.
--
encephalon :: Species
encephalon = Species {
		      averages = Stats { strength=5, dexterity=5, constitution=40, intelligence=40, perception=5, charisma=5, mindfulness=5 },
		      distributions = (stats 15),
		      attribute_generator = [male 0.95,
					     AttributeAlways $ FavoredClass Engineer],
		      species_name = "encephalon"
		     }
		      
		      

-- |
-- These are brightly colored blobs of flesh and brain with eye-stalks and six limbs.
-- The Hellion homeworld is a member of the Interstellar Concordance.
--
hellion :: Species
hellion = Species {
		   averages = Stats { strength=9, dexterity=18, constitution=9, intelligence=11, perception=12, charisma=9, mindfulness=9 },
		   distributions = (stats 20),
		   attribute_generator = [AttributeAlways $ FavoredClass Scout,
					  AttributeAlways $ FavoredClass Marine,
					  AttributeAlways $ FavoredClass Thief,
					  AttributeAlways $ FavoredClass Pirate,
					  male 0.65],
		   species_name = "hellion"
		  }

-- |
-- Large, tough, gray aliens with big heads and big eyes that like to smash.
--
goliath :: Species
goliath = Species {
		   averages = Stats { strength=15, dexterity=9, constitution=15, intelligence=8, perception=10, charisma=6, mindfulness=7 },
		   distributions = (stats 14),
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
kraken :: Species
kraken = Species {
		  averages = Stats { strength=12, dexterity=12, constitution=14, intelligence=10, perception=4, charisma=14, mindfulness=10 },
		  distributions = (stats 12),
		  attribute_generator = ([male 0.5,
					  AttributeAlways $ FavoredClass Consular]), -- also, water survival skill
		  species_name = "kraken"
		 }

-- |
-- Ant-like species.  An inventive species that effectively uses consensus decision making.  They are
-- somehow signatories to the Pan Galactic Treaty Organization even though they have no formal government.
-- In ancient times members of this race were responsible for the destruction of the anachronic homeworld.
--
myrmidon :: Species
myrmidon = Species {
		    averages = Stats { strength=20, dexterity=11, constitution=9, intelligence=14, perception=8, charisma=10, mindfulness=10 },
		    distributions = (stats 14),
		    attribute_generator = [AttributeAlways $ FavoredClass Barbarian,
					   AttributeAlways $ FavoredClass Engineer,
					   AttributeAlways $ FavoredClass Warrior,
					   female 1],
		    species_name = "myrmidon"
		   }

-- |
-- Plant creatures!  Mobile flowering shrubs.  Although their homeword has been a member of the Pan Galactic
-- Treaty Organization since shortly after it was first established, they have never as a group participated in any
-- actions with that organization.
--
perennial :: Species
perennial = Species {
		     averages = Stats { strength=3, dexterity=3, constitution=11, intelligence=11, perception=9, charisma=10, mindfulness=20 },
		     distributions = (stats 20),
		     attribute_generator = ([AttributeAlways $ FavoredClass Barbarian,
					     AttributeAlways $ FavoredClass Consular,
					     AttributeAlways $ FavoredClass Shepherd,
					     AttributeAlways DamageReductionTrait]),
		     species_name = "perennial"
		   }

-- |
-- Recreants are not a single species, but a variety of different self-replicating machines left over from
-- the Myrmidon-Anachronid war.
--
recreant :: Species
recreant = Species {
		    averages = (stats (6)) { strength=14, dexterity=14 },
		    distributions = (stats 13),
		    attribute_generator = ([AttributeAlways $ FavoredClass Barbarian]), -- also: resistance to every energy type escept kinetic
		    species_name = "recreant"
		   }

-- |
-- An adaptable, velociraptor-esque species was genetically engineered for combat in ancient times but 
-- today has developed a culture and unique psychology that allows them to serve as negotiators and peacemakers.
-- The reptilian homeworld is a signatory planet to the Pan Galactic Treaty Organization.
--
reptilian :: Species
reptilian = Species {
		     averages = Stats { strength=11, dexterity=11, constitution=11, intelligence=6, perception=10, charisma=12, mindfulness=6 },
		     distributions = (stats 13),
		     attribute_generator = ([male 0.35,
					     AttributeAlways $ FavoredClass Warrior,
					     AttributeAlways $ FavoredClass Consular] ++
					    (multipleAttribute ToughnessTrait (2,3)) ++
					    (multipleAttribute SpeedTrait (0,2)) ++
					    (multipleAttribute MeleeAttackSkill (2,5))), -- also: vulnerability to cold and fire
		     species_name = "reptilian"
		    }
