module Fib
    (fib,fibList)
    where

import Data.List

-- |
-- The fibonacci sequence beginning with the two specified
-- starting integers.  The most famous fibonacci sequence is
-- genericFib (1,1) = [2,3,5,8,13,21 . . .]
-- The result does not include the leading [1,1].
--
generalFib :: (Integer,Integer) -> [Integer]
generalFib (minusTwo,minusOne) = let zero = minusTwo + minusOne
				     in zero:(generalFib (minusOne,zero))

-- |
-- genericFib with leading two integers.
--
generalFib' :: (Integer,Integer) -> [Integer]
generalFib' (a,b) = a:b:(generalFib (a,b))

-- |
-- (fib n) answers the nth element of the fibonacci sequence.
--
fib :: Integer -> Integer
fib = genericIndex fibList

-- |
-- The fibonacci sequence as a haskell list [1,1,2,3,5,8 . . .].
--
fibList :: [Integer]
fibList = generalFib' (1,1)