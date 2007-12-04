\section{List Manipulation: RSAGL.ListUils}

\begin{code}
module RSAGL.ListUtils
    (doubles,
     loopedDoubles,
     consecutives,
     loopedConsecutives,
     dropRandomElements,
     matrixMultiplyImpl,
     loopList,
     shiftR,
     zeroToOne)
    where

import Data.Array
import System.Random
\end{code}

pairify converts a list of length two into a pair.

\begin{code}
pairify :: [a] -> (a,a)
pairify (m:n:[]) = (m,n)
pairify _ = error "pairify only works on lists of length two"
\end{code}

doubles does this: doubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5)]

\begin{code}
doubles :: [a] -> [(a,a)]
doubles = (map pairify) . (consecutives 2)
\end{code}

loopedDoubles does this: loopedDoubles [1,2,3,4,5] = [(1,2),(2,3),(3,4),(4,5),(5,1)]

\begin{code}
loopedDoubles :: [a] -> [(a,a)]
loopedDoubles = (map pairify) . (loopedConsecutives 2)
\end{code}

consecutives answers a list containing every sequence of n consecutive
elements in the parameter.

consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4]]

\begin{code}
consecutives :: Int -> [a] -> [[a]]
consecutives n xs = let taken = take n xs
			in if (length taken == n)
			   then (taken : (consecutives n $ tail xs))
			   else []
\end{code}

loopedConsecutives answers a list containing every sequence of n consecutive
elements in the parameter, in a circular way so that the first
element of the list is considered subsequent to the last.

consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4],[3,4,1],[4,1,2]]

\begin{code}
loopedConsecutives :: Int -> [a] -> [[a]]
loopedConsecutives n xs = consecutives n $ take (n + length xs - 1) $ cycle xs
\end{code}

dropRandomElements removes some elements of a list at random.  The first parameter is the number of elements out of 100 that should be included (not dropped).  The second is a random seed, and the third is the list to be operated on.

\begin{code}
dropRandomElements :: Int -> StdGen -> [a] -> [a]
dropRandomElements percent _ _ | percent > 100 = error "dropRandomElements: percent > 100"
dropRandomElements percent _ _ | percent < 0 = error "dropRandomElements: percent < 100"
dropRandomElements _ _ [] = []
dropRandomElements percent rand_ints things = 
    let (next_int,next_gen) = next rand_ints
	rest = dropRandomElements percent next_gen (tail things)
	in if (next_int `mod` 100 < percent)
	   then ((head things) : rest)
	   else rest
\end{code}

matrixMultiplyImpl implements matrix multiplication over arbitrary types with arbitrary operations.
\footnote{matrixMultiplyImpl is used by matrixMultiply, \pageref{howtoUseMatrixMultiplyImpl}.}

\begin{code}
matrixMultiplyImpl :: (b -> c -> c) -> c -> (a -> a -> b) -> [[a]] -> [[a]] -> [[c]]
matrixMultiplyImpl addOp zero multiplyOp m n = [[foldr addOp zero $ zipWith multiplyOp m' n' | n' <- n] | m' <- m]
\end{code}

loopList appends the first element of a list to the end of the list.  The result is a list of one greater length.

shiftR shifts every element of a list to the right, recyling the last element as the first.  The result is a list of the same length.

\begin{code}
loopList :: [a] -> [a]
loopList ps = ps ++ [head ps]

shiftR :: [a] -> [a]
shiftR ps = last ps : init ps
\end{code}

zeroToOne creates a list of numbers from 0.0 to 1.0, using n steps.

\begin{code}
zeroToOne :: Integer -> [Double]
zeroToOne n | n <= 1000 = ztos ! n
zeroToOne n = zeroToOnePrim n

zeroToOnePrim :: Integer -> [Double]
zeroToOnePrim n = map (*x) [0..(fromInteger $ n-1)]
    where x = recip (fromInteger $ n - 1)

ztos :: Array Integer [Double]
ztos = listArray (0,1000) $ map (zeroToOnePrim) [0 .. 1000]
\end{code}