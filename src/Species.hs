module Species
    (generateCreatureData)
    where

import DB
import Control.Monad
import SpeciesData
import Stats
import Attribute

--
-- Randomly generates a new creature.
--
generateCreatureData :: Species -> DB CreatureGenerationData
generateCreatureData species = do new_stats <- generateStats (averages species) (distributions species) 
				  new_attribs <- generateAttributes (attribute_generator species) 
				  return ( new_stats, new_attribs, (species_name species) )
