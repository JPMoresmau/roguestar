\section{RSAGL.StatefulArrow}

A StatefulArrow is a form of automata or self-modifying program.

The assumption is that a StatefulArrow will be re-evaluated many times.
The result of each iteration includes a new form of the StatefulArrow
that will be evaluated on the next iteration.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.StatefulArrow
    (StatefulArrow(..),
     StatefulFunction,
     stateContext,
     runStateMachine)
    where

import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Arrow.Operations
import Control.Arrow.Transformer

data StatefulArrow a i o = StatefulArrow { runStatefulArrow :: (a i (o,StatefulArrow a i o)) }

instance (Arrow a) => Arrow (StatefulArrow a) where
    (>>>) (StatefulArrow sf1) (StatefulArrow sf2) = StatefulArrow $
        proc a -> do (b,sf1') <- sf1 -< a
                     (c,sf2') <- sf2 -< b
                     returnA -< (c,sf1' >>> sf2')
    arr = lift . arr
    first (StatefulArrow sf) = StatefulArrow $
        proc (b,d) -> do (c,sf') <- sf -< b
                         returnA -< ((c,d),first sf')

type StatefulFunction = StatefulArrow (->)

instance (Arrow a) => ArrowTransformer StatefulArrow a where
    lift f = StatefulArrow $ f &&& (arr $ const $ lift f)
\end{code}

\subsection{Embedding a StateArrow as a StatefulArrow}

stateContext allows a StateArrow to be run as a StatefulArrow, where the StateArrow's 
explicit state becomes the StatefulArrow's implicit state.

\begin{code}
stateContext :: (Arrow a) => StateArrow s a i o -> s -> StatefulArrow a i o
stateContext sa s = StatefulArrow $
    proc i -> do (o,s') <- runState sa -< (i,s)
                 returnA -< (o,stateContext sa s')
\end{code}

\subsection{Using a StatefulArrow as a state machine}

\begin{code}
runStateMachine :: (ArrowChoice a,ArrowApply a) => StatefulArrow a i o -> a [i] [o]
runStateMachine stateful_arrow = 
    proc x -> do runStateMachine_ -< (([],stateful_arrow),x)
        where runStateMachine_ :: (ArrowChoice a,ArrowApply a) => a (([o],StatefulArrow a i o),[i]) [o]
              runStateMachine_ =
                  proc ((reversed_so_far,StatefulArrow stateful),x) -> 
                      do case x of
                                [] -> returnA -< reverse reversed_so_far
                                (i:is) -> do (o,stateful') <- app -< (stateful,i)
                                             runStateMachine_ -< ((o:reversed_so_far,stateful'),is)
\end{code}