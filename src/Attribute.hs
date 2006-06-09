module Attribute
    (generateAttributes)
    where

import AttributeData
import DB
import Dice
import Data.Maybe

--
-- Randomly generate 1 attribute from an attribute generator.
--
generate1Attribute :: AttributeGenerator a -> DB (Maybe a)
generate1Attribute (AttributeAlways someAttrib) = do return (Just someAttrib)
generate1Attribute (AttributeSometimes someAttrib percentage_chance maybeNextGen) = do percentage_roll <- (1 `d` 100)
										       if (percentage_roll <= percentage_chance)
											  then return (Just someAttrib)
											  else case maybeNextGen of
														 Just nextGen -> generate1Attribute nextGen
														 Nothing -> return Nothing

-- 
-- Randomly generate attributes from a list of AttributeGenerators.
--
generateAttributes :: [AttributeGenerator a] -> DB [a]
generateAttributes attribGens = do maybeAttribs <- mapM generate1Attribute attribGens
				   return $ map fromJust $ filter isJust maybeAttribs