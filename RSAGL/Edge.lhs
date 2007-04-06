\section{Edge Detection in FRP programs}

Edge detection works for all inputs that implement Eq, and is simply a mechanism to observe when a value changes.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.Edge 
    (Edge(..),
     edge,
     edgep,
     edgeFold,
     edgeMap,
     history,
     initial,
     started)
    where

import RSAGL.FRP as FRP
import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.Time
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State

\end{code}

\subsection{Edge data structure}

\begin{code}
data Edge a = Edge { edge_previous :: a,
                     edge_next :: a,
                     edge_changed :: Time }
              deriving (Eq,Show)
\end{code}

\subsection{Edge detectors}

edge watches an input and answers an Edge data structure.

\begin{code}
edge :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => FRP i o a e (Edge e)
edge = proc i ->
    do t <- threadTime -< ()
       FRP.statefulContext $ SwitchedArrow.withState edge' initial_edge -< (t,i)
    where initial_edge (t,i) = Edge { edge_previous = i, 
                                      edge_next = i, 
                                      edge_changed = t }
          edge' = proc (t,i) ->
              do e <- lift fetch -< ()
                 lift store -< if i /= edge_next e
                               then Edge { edge_previous = edge_next e,
                                           edge_next = i,
                                           edge_changed = t }
                               else e
                 lift fetch -< ()
\end{code}

edgep answers True exactly once each time a value changes.

\begin{code}
edgep :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => FRP i o a e Bool
edgep = FRP.statefulContext $ SwitchedArrow.withState edgep' id
    where edgep' = proc i ->
              do old_value <- lift fetch -< ()
                 lift store -< i
                 returnA -< i /= old_value
\end{code}

\subsection{Edge folds}

edgeFold combines each unique input into a cumulative value using a folding function.
The folding function must have some way to discard old edges, or it will represent a space leak.

\begin{code}
edgeFold :: (Arrow a,ArrowChoice a,ArrowApply a,Eq j) => p -> (j -> p -> p) -> FRP i o a j p
edgeFold initial_value f = FRP.statefulContext $ SwitchedArrow.withState edgeFold' (\i -> (i,f i initial_value))
    where edgeFold' = proc i ->
              do (old_raw,old_folded) <- lift fetch -< ()
                 let (new_raw,new_folded) = if old_raw == i
                                            then (old_raw,old_folded)
                                            else (i,f i old_folded)
                 lift store -< (new_raw,new_folded)
                 returnA -< new_folded
\end{code}

history answers a history of edges for a value.  The Time parameter indicates the maximum age of an edge, 
after which it can be forgotten.

\begin{code}
history :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => Time -> FRP i o a e [Edge e]
history t = edgeFold [] history' <<< edge
    where history' n h = n : takeWhile ((>= edge_changed n - t) . edge_changed) h
\end{code}

edgeMap is equivalent to \texttt{arr}, but more efficient, possibly, because the mapping function is only 
applied when the input changes.

edgeMap should actually be more efficient only when: the cost of the mapping function is high, the cost
of comparing two equal values is low, and the input changes infrequently.

\begin{code}
edgeMap :: (Arrow a,ArrowChoice a,ArrowApply a,Eq j) => (j -> p) -> FRP i o a j p
edgeMap f = edgeFold undefined (\i -> \_ -> f i)
\end{code}

\subsection{Remembering the initial value of an input}

initial answers the first value that an input ever has (during this instance of this thread).

\begin{code}
initial :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => FRP i o a e e
initial = FRP.statefulContext $ SwitchedArrow.withState initial1 undefined
    where initial1 = proc i ->
              do lift store -< i
                 SwitchedArrow.switchContinue -< (initial2,i)
          initial2 = lift fetch
\end{code}

started is the absoluteTime at which this thread started.

\begin{code}
started :: (Arrow a,ArrowChoice a,ArrowApply a) => FRP i o a () Time
started = initial <<< absoluteTime
\end{code}