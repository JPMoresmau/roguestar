module SpeciesData
    (percentMale,
     percentFemale,
     exampleSpecies,
     Species(..),
     CreatureGenerationData)
    where

import StatsData
import CreatureData
import AttributeData

--
-- Makes the creature male x percent of the time (female otherwise).
--
percentMale :: Integer -> AttributeGenerator CreatureAttribute
percentMale x = AttributeSometimes Male x $ Just (AttributeAlways Female)

--
-- Makes the creature female x percent of the time (male otherwise).
--
percentFemale :: Integer -> AttributeGenerator CreatureAttribute
percentFemale x = AttributeSometimes Female x $ Just (AttributeAlways Male)

data Species = Species { averages :: Stats, 
                         distributions :: Stats,
                         attribute_generator :: [AttributeGenerator CreatureAttribute],
                         species_name :: String } 
	       deriving (Show, Read)

--
-- Tuple that contains generated data for a new creature.  Contains the stats for the new creature,
-- the attributes, and the name of the creature's species.
--
type CreatureGenerationData = ( Stats, [CreatureAttribute], String )

--
-- An example species.
--
exampleSpecies :: Species
exampleSpecies = Species {
			  averages = Stats { str=1, dex=(-2), con=1, int=(-1), per=(-1), cha=3, mind=(-1) },
			  distributions = (stats 2),
			  attribute_generator = [percentMale 40],
			  species_name = "Example-Species" 
			 }
