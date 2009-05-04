\section{RSAGL.StatefulArrow}

A StatefulArrow is a form of automata or self-modifying program.

The assumption is that a StatefulArrow will be re-evaluated many times.
The result of each iteration includes a new form of the StatefulArrow
that will be evaluated on the next iteration.

\begin{code}
{-# LANGUAGE Arrows, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

module RSAGL.FRP.StatefulArrow
    (StatefulArrow(..),
     StatefulFunction,
     stateContext,
     withState,
     withExposedState,
     statefulTransform,
     runStateMachine)
    where

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Category

type StatefulFunction = StatefulArrow (->)
data StatefulArrow a i o = StatefulArrow { runStatefulArrow :: (a i (o,StatefulArrow a i o)) }

instance (Category a,Arrow a) => Category (StatefulArrow a) where
    (.) (StatefulArrow sf2) (StatefulArrow sf1) = StatefulArrow $
        proc a -> do (b,sf1') <- sf1 -< a
                     (c,sf2') <- sf2 -< b
                     returnA -< seq b $ seq c $ seq sf1' $ seq sf2' $ (c,sf1' >>> sf2')
    id = lift id

instance (Arrow a) => Arrow (StatefulArrow a) where
    arr = lift . arr
    first (StatefulArrow sf) = StatefulArrow $
        proc (b,d) -> do (c,sf') <- sf -< b
                         returnA -< seq c $ seq sf' $ ((c,d),first sf')
    second (StatefulArrow sf) = StatefulArrow $
        proc (d,b) -> do (c,sf') <- sf -< b
                         returnA -< seq c $ seq sf' $ ((d,c),second sf')

instance (Arrow a) => ArrowTransformer StatefulArrow a where
    lift f = lifted
        where lifted = StatefulArrow $ f &&& (arr $ const $ lifted)
\end{code}

\subsection{Mixing StatefulArrows and StateArrows}
\label{withState}
\label{withExposedState}

stateContext allows a StateArrow to be run as a StatefulArrow, where the StateArrow's 
explicit state becomes the StatefulArrow's implicit state.

withState allows a StatefulArrow to be both explicitly and implicitly stateful.
The StatefulArrow does the work of retaining the explicit state between iterations.

withExposedState exposes the state to the caller by explicitly routing the state
as an input and output of the arrow.

\begin{code}
stateContext :: (Arrow a) => StateArrow s a i o -> s -> StatefulArrow a i o
stateContext sa s = StatefulArrow $
    proc i -> do (o,s') <- runState sa -< (i,s)
                 returnA -< seq o $ seq s' $ (o,stateContext sa s')

withState :: (Arrow a,ArrowApply a) => StatefulArrow (StateArrow s a) i o -> s -> StatefulArrow a i o
withState sa s = flip stateContext (sa,s) $
    proc i -> do (StatefulArrow sa',s') <- fetch -< ()
                 ((o,sa''),s'') <- lift app -< (runState sa',(i,s'))
                 store -< seq sa'' $ seq s'' $ seq o $ (sa'',s'')
                 returnA -< seq sa'' $ seq s'' o

withExposedState :: (Arrow a,ArrowApply a) => StatefulArrow (StateArrow s a) i o -> StatefulArrow a (i,s) (o,s)
withExposedState (StatefulArrow sa) = StatefulArrow $ (arr $ \((o,sa'),s') -> ((o,s'),withExposedState sa')) <<< runState sa
\end{code}

\subsection{Transforming the underlying type of a StatefulArrow}

\begin{code}
statefulTransform :: (Arrow a,Arrow b) => (forall j p. a j p -> b j p) -> 
                                          StatefulArrow a i o -> StatefulArrow b i o
statefulTransform f (StatefulArrow a) = StatefulArrow $
    proc i -> do (o,a') <- f a -< i
                 returnA -< seq o $ seq a' $ (o,statefulTransform f a')
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
