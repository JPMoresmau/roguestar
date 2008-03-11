module AttributeData
    (AttributeGenerator(..),
    percentAttribute,
    multipleAttribute)
    where

import Data.List

-- |
-- Used to randomly generate attributes for an entity.
-- AttributeAlways is a generator that always creates the specified attribute.
-- (AttributeSometimes attrib x $ otherwise) is a generator that generates
-- the the attribute "attrib" x-fraction of the time, and invokes the attribute
-- generator "otherwise" otherwise.
--

data AttributeGenerator a = AttributeAlways a
                          | AttributeSometimes a Rational (Maybe (AttributeGenerator a))
                            deriving (Show, Read)

-- |
-- Grants the entity the specified attribute x percent of the time, otherwise nothing
--
percentAttribute :: a -> Rational -> AttributeGenerator a
percentAttribute attr x = AttributeSometimes attr x $ Nothing

-- |
-- Grants the entity the specified attribute between minimum and maximum instances of the
-- attribute, on average the average of the two (as a binomial distribution).
--
multipleAttribute :: a -> (Integer,Integer) -> [AttributeGenerator a]
multipleAttribute attr (mini,maxi) | mini >= 0 && maxi >= mini = 
    (genericReplicate mini $ AttributeAlways attr) ++ (genericReplicate (maxi-mini) $ percentAttribute attr 50)
multipleAttribute _ _ = error "multipleAttribute: maximum < minimum badness"
