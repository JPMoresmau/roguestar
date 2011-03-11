-- | Generates random lists of specific data points "attributes" of any data type.
-- The attributes themselves aren't random, only their arrangement and frequency within the list.
--
module AttributeGeneration
    where

import Data.Ratio
import Data.List
import Control.Monad.Random
import Data.Monoid
import Control.Monad

-- | Description of the random data to be generated.
data AttributeGenerator a =
      AttributeAlways {
          attribute_actual :: a,
          attribute_min_max :: (Integer,Integer) }
    | AttributeChoice {
          attribute_frequency :: Rational,
          attribute_yes :: [AttributeGenerator a],
          attribute_no :: [AttributeGenerator a] }

instance Monoid (AttributeGenerator a) where
    mempty = AttributeChoice {
        attribute_frequency = 0,
        attribute_yes = [],
        attribute_no = [] }
    mappend a b = mconcat [a,b]
    mconcat as = AttributeChoice {
        attribute_frequency = 1,
        attribute_yes = as,
        attribute_no = [] }

-- | Generate exactly n copies of an attribute.
attributeStatic :: Integer -> a -> AttributeGenerator a
attributeStatic n a =attributeMinMax (n,n) a

-- | Generates between a random number of copies of an attribute between a lower and upper bound.
attributeMinMax :: (Integer,Integer) -> a -> AttributeGenerator a
attributeMinMax min_max a = AttributeAlways {
    attribute_actual = a,
    attribute_min_max = min_max }

-- | Generates the first class of attributes some fraction of the time, and the other list the remainder of the time.
-- For example 'attributeChoice (1%3) [attributeStatic 1 True] [attributeStatic 1 False]' would generate 'True' 33% of the time.
attributeChoice :: Rational -> [AttributeGenerator a] -> [AttributeGenerator a] -> AttributeGenerator a
attributeChoice freq yes no = AttributeChoice {
    attribute_frequency = freq,
    attribute_yes = yes,
    attribute_no = no }

-- | A set of mutually-exclusive choices, with Integer probability weights.
attributeChoices :: [(Integer,[AttributeGenerator a])] -> AttributeGenerator a
attributeChoices [] = mempty
attributeChoices (x:xs) = attributeChoice (fst x % (sum $ map fst $ x:xs)) (snd x) [attributeChoices xs]

-- | Run the 'AttributeGenerator'.
generateAttributes :: (MonadRandom m) => AttributeGenerator a -> m [a]
generateAttributes (AttributeAlways { attribute_actual = a, attribute_min_max = min_max }) =
    do n <- getRandomR min_max
       return $ genericReplicate n a
generateAttributes (AttributeChoice { attribute_frequency = l, attribute_yes = yes, attribute_no = no }) =
    do n <- getRandomR (1,denominator l)
       case () of
           () | n <= numerator l -> liftM concat $ mapM generateAttributes yes
           () | otherwise -> liftM concat $ mapM generateAttributes no
