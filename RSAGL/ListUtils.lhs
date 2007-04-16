\section{List Manipulation: RSAGL.ListUils}

\begin{code}
module RSAGL.ListUtils
    (doubles,
     loopedDoubles,
     consecutives,
     loopedConsecutives,
     dropRandomElements)
    where

import System.Random
\end{code}

pairify converts a list of length two into a list of tuple pairs.

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
consecutives n elems = let taken = take n elems
			   in if (length taken == n)
			      then (taken : (consecutives n $ tail elems))
			      else []
\end{code}


loopedConsecutives answers a list containing every sequence of n consecutive
elements in the parameter, in a circular way so that the first
element of the list is considered subsequent to the last.

consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4],[3,4,1],[4,1,2]]

\begin{code}
loopedConsecutives :: Int -> [a] -> [[a]]
loopedConsecutives n elems = consecutives n $ take (n + length elems - 1) $ cycle elems
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