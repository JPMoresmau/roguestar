module Races
    (selectPlayerRace,
     player_race_names,
     all_races,
     allowed_player_races,
     anachronid,
     male_anachronid,
     female_anachronid,
     cyborg,
     goliath,
     hellion,
     human,
     myrmidon,
     reptilian,
     planetar,
     shylock,
     synthanthrope)
    where

import Data.Char
import StatsData
import CreatureData
import SpeciesData
import AttributeData
import Data.List

all_races :: [Species]
all_races = [anachronid,
	     cyborg,
	     goliath,
	     hellion,
	     human,
	     kraken,
	     myrmidon,
	     reptilian,
	     planetar,
	     shylock,
	     synthanthrope]

allowed_player_races :: [Species]
allowed_player_races = [female_anachronid,
			goliath,
			hellion,
			human,
			kraken,
			myrmidon,
			reptilian,
			planetar,
			shylock,
			synthanthrope]

player_race_names :: [String]
player_race_names = map (map toLower . species_name) allowed_player_races

selectPlayerRace :: String -> Maybe Species
selectPlayerRace race_name = find 
			     (\x -> (map toLower $ species_name x) == map toLower race_name) 
			     allowed_player_races

-- |
-- Six-legged species that move through time unusually slowly, making them appear (to outsiders),
-- to move very quickly.  Yes, they eat their own males -- squad leaders are always female.
--
anachronid :: Species
anachronid = Species {
		      averages = Stats { str=(-3), dex=3, con=(-2), int=(-1), per=(-4), cha=1, mind=3 },
		      distributions = (stats 4),
		      attribute_generator = ([percentFemale 5] ++ 
		                             (multipleAttribute Speed (5,14))),
		      species_name = "anachronid"
		     }

female_anachronid :: Species
female_anachronid = anachronid { attribute_generator = [percentFemale 100] ++ (attribute_generator anachronid) }

male_anachronid :: Species
male_anachronid = anachronid { attribute_generator = [percentMale 100,AttributeAlways NoKillPenalty] ++ (attribute_generator anachronid) }

-- |
-- Humanoids that have been augmented with cybernetic technology.  Very strong, tough, and intelligent.
-- (Always evil strategic).
--
cyborg :: Species
cyborg = Species {
		  averages = Stats { str=15, dex=(-10), con=15, int=15, per=(-10), cha=(-10), mind=(-10) },
		  distributions = (stats 10) { int=0, per=4, cha=4, mind=4 },
		  attribute_generator = ([percentMale 50,
					  AttributeAlways NoKillPenalty,
					  AttributeAlways DoesNotNegotiate] ++
					 (multipleAttribute Evasion (5,10)) ++
					 (multipleAttribute ImprovedRangedCombat (5,10) ++)
					 (multipleAttribute Toughness (5,10))),
		  species_name = "cyborg"
		 }

-- |
-- Small humanoids with four arms.
--
hellion :: Species
hellion = Species {
		   averages = Stats { str=(-1), dex=0, con=0, int=(-2), per=4, cha=3, mind=(-2) },
		   distributions = (stats 3),
		   attribute_generator = [percentMale 65],
		   species_name = "hellion"
		  }

-- |
-- Your basic, average humanoid species.
--
human :: Species
human = Species {
		 averages = (stats 0),
		 distributions = (stats 4),
		 attribute_generator = ([percentMale 50]),
		 species_name = "human"
		}

-- |
-- Large, tough, gray aliens with big heads and big eyes that like to smash.
--
goliath :: Species
goliath = Species {
		   averages = Stats { str=6, dex=(-2), con=9, int=(-5), per=2, cha=(-6), mind=(-6) },
		   distributions = (stats 6),
		   attribute_generator = ([percentMale 75]),
		   species_name = "goliath"
		  }

-- |
-- Aquatic species with tenticles.
--
kraken :: Species
kraken = Species {
		  averages = Stats { str=2, dex=2, con=4, int=0, per=(-2), cha=4, mind=0 },
		  distributions = (stats 2) { mind=6 },
		  attribute_generator = ([percentMale 50,
					  AttributeAlways WaterSurvival]),
		  species_name = "kraken"
		 }

-- |
-- Ant-like species.  Inventive species that works well in groups despite complete lack of centralized
-- leadership.  Tagline: "I find your proposition unreasonable."
--
myrmidon :: Species
myrmidon = Species {
		    averages = Stats { str=(-6), dex=2, con=(-6), int=10, per=(-4), cha=0, mind=0 },
		    distributions = (stats 4),
		    attribute_generator = ([percentFemale 100]),
		    species_name = "myrmidon"
		   }

-- |
-- Velociraptor-esque species that likes to claw things to death.
--
reptilian :: Species
reptilian = Species {
		     averages = Stats { str=1, dex=4, con=3, int=(-6), per=4, cha=(-3), mind=(-2) },
		     distributions = (stats 3),
		     attribute_generator = ([percentMale 35] ++
					    (multipleAttribute Toughness (1,2)) ++
					    (multipleAttribute Speed (0,2)) ++
					    (multipleAttribute ImprovedMeleeCombat (4,10))),
		     species_name = "reptilian"
		    }

-- |
-- Plant creatures!  Highly enlightened plant creatures and founders of the Interplanetary Concordance.
--
planetar :: Species
planetar = Species {
		    averages = Stats { str=(-4), dex=(-4), con=2, int=2, per=(-1), cha=2, mind=5 },
		    distributions = (stats 5),
		    attribute_generator = ([AttributeAlways DamageReduction,
					    AttributeAlways HideInForest,
					    AttributeAlways WaterSurvival]),
		    species_name = "planetar"
		   }

-- |
-- Serpentine race generally known as the black marketeers of the galaxy.  This is somewhat of a 
-- misunderstanding:  Among Shylocks, actual honesty -- especially in business dealings -- is 
-- considered an insult.
--
shylock :: Species
shylock = Species {
		   averages = Stats { str=(-2), dex=(-1), con=(-1), int=1, per=3, cha=4, mind=(-3) },
		   distributions = (stats 6),
		   attribute_generator = ([percentMale 50]),
		   species_name = "shylock"
		  }

-- |
-- Androids.  The first member of their species was a creation of a mad scientist, after hundreds
-- of years this one individual developed into an entire race.  They have few emotions.
--
synthanthrope :: Species
synthanthrope = Species {
			 averages = (stats 2) { int=6, cha=(-4) },
			 distributions = (stats 0),
			 attribute_generator = ([AttributeAlways DoesNotValueMoney] ++
						(multipleAttribute DamageReduction (3,3))),
			 species_name = "synthanthrope"
			}
