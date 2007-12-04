\section{Generic Search Algorithms}

\begin{code}
module RSAGL.Search
    (search,
     bestFirst)
    where

import Debug.Trace
import Data.Set as Set
import Data.List as List
\end{code}

\subsection{Search Algorithms}

The basic function, \texttt{search}, takes a search algorithm, a branching function, and a value function, and searches the tree definined by a starting value and the branching function for the node with the greatest value.

\begin{code}
search :: ([SearchTree a] -> SearchTree a) -> (a -> [a]) -> (a -> Double) -> [a] -> a
search algorithmFn branchFn valueFn starting_nodes = node $ algorithmFn $ List.map (constructTree branchFn valueFn) starting_nodes
\end{code}

\texttt{bestFirst} is a search algorithm that expands the most valued nodes first on the theory that highly values nodes will expand to even higher valued nodes.  The first parameter limits the maximum number of steps that the search will take.  This search function is generalized to handle infinite search trees and search trees that are actually graphs containing cycles, and optimized for nodes

\texttt{bestFirst} does not try to expand nodes that have a value of 0 or less.

\begin{code}
bestFirst :: (Eq a,Ord a,Show a) => Integer -> [SearchTree a] -> SearchTree a
bestFirst = beamSearch 1

beamSearch :: (Eq a,Ord a,Show a) => Integer -> Integer -> [SearchTree a] -> SearchTree a
beamSearch x _ _ | x < 1 = error "beamSearch: beam_width < 1"
beamSearch _ x _ | x < 1 = error "beamSearch: max_steps < 1"
beamSearch beam_width max_steps starting_nodes = maximumBy compareByValue $ List.map (\x -> trace (show (node x) ++ " " ++ show (node_value x)) x) $ genericTake max_steps $ 
                                         concat $ unfoldr expandStep (Set.fromList starting_nodes,empty)
    where expandStep (nodes,_) | Set.null nodes = Nothing
          expandStep (nodes,bests) = let m = List.filter ((/= 0) . node_value) $ genericTake beam_width $ 
                                                 reverse $ sortBy compareByValue $ toList nodes
                                         ms = fromList m
                                         bs = ms `Set.union` bests
                                         s = (nodes `Set.union` (fromList $ concatMap node_children m)) `difference` bs
                                             in case m of
                                                 [] -> Just (m,(empty,empty))
                                                 _ -> Just (m,(s,bs))
\end{code}

\subsection{Constructing SearchTrees}

\begin{code}
data SearchTree a = SearchTree
   { node :: a,
     node_value :: Double,
     node_children :: [SearchTree a] }

constructTree :: (a -> [a]) -> (a -> Double) -> a -> SearchTree a
constructTree branchFn valueFn starting_node =
   SearchTree {
       node = starting_node,
       node_value = valueFn starting_node,
       node_children = List.map (constructTree branchFn valueFn) $ branchFn starting_node }

compareByValue :: SearchTree a -> SearchTree a -> Ordering
compareByValue x y = compare (node_value x) (node_value y)

instance (Eq a) => Eq (SearchTree a) where
    x == y = node x == node y

instance (Ord a) => Ord (SearchTree a) where
    compare x y = compare (node x) (node y)
\end{code}