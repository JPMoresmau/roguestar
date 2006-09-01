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
	     kraken,
	     goliath,
	     hellion,
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
			kraken,
			goliath,
			hellion,
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
		      averages = Stats { str=1, dex=1, con=0, int=0, per=0, cha=(-2), mind=0 },
		      distributions = (stats 3),
		      attribute_generator = ([percentFemale 5,
					      AttributeAlways $ FavoredClass Barbarian] ++ 
		                             (multipleAttribute Speed (5,14))),
		      species_name = "anachronid"
		     }

female_anachronid :: Species
female_anachronid = anachronid { attribute_generator = [percentFemale 100] ++ (attribute_generator anachronid) }

male_anachronid :: Species
male_anachronid = anachronid { attribute_generator = [percentMale 100,AttributeAlways NoKillPenalty] ++ (attribute_generator anachronid) }

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
		      averages = (stats (-1)) { int=7, cha=(-2) },
		      distributions = (stats 0) { int=0 },
		      attribute_generator = ([AttributeAlways DoesNotValueMoney,
					      AttributeAlways $ FavoredClass Engineer] ++
					     (multipleAttribute DamageReduction (3,3))), --also: some resistance to kinetic energy
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
		     averages = Stats { str=(-4), dex=(-1), con=(-1), int=2, per=(-1), cha=1, mind=4 },
		     distributions = (stats 4),
		     attribute_generator = [AttributeAlways $ FavoredClass Shepherd,
					    percentMale 45], -- also: very high resistance to kinetic energy
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
		      averages = Stats { str=(-1), dex=2, con=(-1), int=(-2), per=(-2), cha=2, mind=2 },
		      distributions = (stats 5),
		      attribute_generator = [percentMale 60,
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
		      averages = Stats { str=(-10), dex=(-10), con=20, int=20, per=(-10), cha=(-5), mind=(-5) },
		      distributions = (stats 5),
		      attribute_generator = [percentMale 95,
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
		   averages = Stats { str=(-1), dex=1, con=(-1), int=1, per=2, cha=(-1), mind=(-1) },
		   distributions = (stats 10),
		   attribute_generator = [AttributeAlways $ FavoredClass Scout,
					  AttributeAlways $ FavoredClass Marine,
					  AttributeAlways $ FavoredClass Thief,
					  percentMale 65],
		   species_name = "hellion"
		  }

-- |
-- Large, tough, gray aliens with big heads and big eyes that like to smash.
--
-- Homeworld: Golia-Indri
--
goliath :: Species
goliath = Species {
		   averages = Stats { str=3, dex=(-1), con=4, int=(-2), per=0, cha=(-4), mind=(-3) },
		   distributions = (stats 4),
		   attribute_generator = ([percentMale 55,
					   AttributeAlways $ FavoredClass Barbarian,
					   AttributeAlways $ FavoredClass Warrior,
					   AttributeAlways $ FavoredClass Scout] ++
					  (multipleAttribute Toughness (3,7))),
		   species_name = "goliath"
		  }

-- |
-- Aquatic species with tenticles.  The kraken homeworld is the capital of the Imperial Aliance.
--
-- Homeworld: Quatica
--
kraken :: Species
kraken = Species {
		  averages = Stats { str=2, dex=2, con=4, int=0, per=(-2), cha=4, mind=0 },
		  distributions = (stats 2) { mind=6 },
		  attribute_generator = ([percentMale 45,
					  AttributeAlways $ FavoredClass Privateer,
					  AttributeAlways $ FavoredClass Consular,
					  AttributeAlways WaterSurvival]),
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
		    averages = Stats { str=2, dex=(-1), con=(-1), int=4, per=(-2), cha=(-2), mind=0 },
		    distributions = (stats 4),
		    attribute_generator = [AttributeAlways $ FavoredClass Barbarian,
					   AttributeAlways $ FavoredClass Engineer,
					   AttributeAlways $ FavoredClass Warrior,
					   percentFemale 100],
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
		     averages = Stats { str=(-4), dex=(-4), con=1, int=1, per=(-1), cha=2, mind=2 },
		     distributions = (stats 5),
		     attribute_generator = ([AttributeAlways $ FavoredClass Barbarian,
					     AttributeAlways $ FavoredClass Consular,
					     AttributeAlways $ FavoredClass Shepherd,
					     AttributeAlways DamageReduction,
					     AttributeAlways ForestSurvival,
					     AttributeAlways WaterSurvival,
					     AttributeAlways Regeneration,
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
		    averages = (stats (-4)) { dex=4 },
		    distributions = (stats 3),
		    attribute_generator = ((multipleAttribute Regeneration (15,25)) ++
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
		     averages = Stats { str=1, dex=1, con=1, int=(-4), per=0, cha=2, mind=(-4) },
		     distributions = (stats 3),
		     attribute_generator = ([percentMale 35,
					     AttributeAlways $ FavoredClass Warrior,
					     AttributeAlways $ FavoredClass Consular] ++
					    (multipleAttribute Toughness (2,3)) ++
					    (multipleAttribute Speed (0,2)) ++
					    (multipleAttribute ImprovedMeleeCombat (2,5))), -- also: vulnerability to cold and fire
		     species_name = "reptilian"
		    }