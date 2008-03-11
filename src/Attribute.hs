
module Attribute
    (generateAttributes)
    where

import AttributeData
import DB
import Dice
import Data.Maybe
import Data.Ratio

-- |
-- Randomly generate 1 attribute from an attribute generator.
--
generate1Attribute :: AttributeGenerator a -> DB (Maybe a)
generate1Attribute (AttributeAlways someAttrib) = do return (Just someAttrib)
generate1Attribute (AttributeSometimes someAttrib chance maybeNextGen) = 
     do good <- roll $ map (<= numerator chance) [1..denominator chance]
        if good
           then return (Just someAttrib)
           else case maybeNextGen of
                    Just nextGen -> generate1Attribute nextGen
                    Nothing -> return Nothing

-- |
-- Randomly generate attributes from a list of AttributeGenerators.
--
generateAttributes :: [AttributeGenerator a] -> DB [a]
generateAttributes attribGens = 
    do maybeAttribs <- mapM generate1Attribute attribGens
       return $ map fromJust $ filter isJust maybeAttribs
