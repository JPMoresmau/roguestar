module ListUtils (count)
    where

import Data.List

-- |
-- count 1 [2,5,1,4,1,1] is 3, because 1 occurs three times.
--
count :: Eq a => a -> [a] -> Integer
count elem lst = genericLength $ elemIndices elem lst
