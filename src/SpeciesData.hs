
module SpeciesData
    (male,
     female,
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
male :: Rational -> AttributeGenerator CreatureAttribute
male x = AttributeSometimes (Gender Male) x $ Just (AttributeAlways (Gender Female))

--
-- Makes the creature female x percent of the time (male otherwise).
--
female :: Rational -> AttributeGenerator CreatureAttribute
female x = AttributeSometimes (Gender Female) x $ Just (AttributeAlways (Gender Male))

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
			  averages = Stats { strength=1, dexterity=(-2), constitution=1, intelligence=(-1), perception=(-1), charisma=3, mindfulness=(-1) },
			  distributions = (stats 2),
			  attribute_generator = [male 0.4],
			  species_name = "Example-Species" 
			 }
