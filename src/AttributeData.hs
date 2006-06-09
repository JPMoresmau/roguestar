module AttributeData
    (AttributeGenerator(..),
    percent_attribute)
    where

--
-- Used to randomly generate attributes for an entity.
-- AttributeAlways is a generator that always creates the specified attribute.
-- (AttributeSometimes attrib x $ otherwise) is a generator that generates
-- the the attribute "attrib" x-percent of the time, and invokes the attribute
-- generator "otherwise" otherwise.
--

data AttributeGenerator a = AttributeAlways a
                          | AttributeSometimes a Integer (Maybe (AttributeGenerator a))
                            deriving (Show, Read)

--
-- Grants the creature the specified attribute x percent of the time, otherwise nothing
--
percent_attribute :: a -> Integer -> AttributeGenerator a
percent_attribute attr x = AttributeSometimes attr x $ Nothing