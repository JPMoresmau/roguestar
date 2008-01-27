\section{RSAGL.StatefulArrow}

A StatefulArrow is a form of automata or self-modifying program.

The assumption is that a StatefulArrow will be re-evaluated many times.
The result of each iteration includes a new form of the StatefulArrow
that will be evaluated on the next iteration.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.StatefulArrow
    (StatefulArrow(..),
     runStatefulArrow,
     StatefulFunction,
     stateContext,
     withState,
     withExposedState,
     statefulTransform,
     runStateMachine)
    where

import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Arrow.Operations
import Control.Arrow.Transformer

type StatefulFunction = StatefulArrow (->)
data StatefulArrow a i o = StatefulArrow (a i (o,StatefulArrow a i o))
                         | LiftedStatefulArrow (a i o)

instance (Arrow a) => Arrow (StatefulArrow a) where
    (>>>) (LiftedStatefulArrow lsf1) (LiftedStatefulArrow lsf2) = LiftedStatefulArrow (lsf1 >>> lsf2)
    (>>>) (LiftedStatefulArrow lsf1) sf2 = liftPrim lsf1 >>> sf2
    (>>>) sf1 (LiftedStatefulArrow lsf2) = sf1 >>> liftPrim lsf2
    (>>>) (StatefulArrow sf1) (StatefulArrow sf2) = StatefulArrow $
        proc a -> do (b,sf1') <- sf1 -< a
                     (c,sf2') <- sf2 -< b
                     returnA -< (c,sf1' >>> sf2')
    arr = lift . arr
    first (StatefulArrow sf) = StatefulArrow $
        proc (b,d) -> do (c,sf') <- sf -< b
                         returnA -< ((c,d),first sf')
    first (LiftedStatefulArrow lsf) = LiftedStatefulArrow (first lsf)

instance (Arrow a) => ArrowTransformer StatefulArrow a where
    lift = LiftedStatefulArrow

liftPrim :: (Arrow a) => a i o -> StatefulArrow a i o
liftPrim f = StatefulArrow $ f &&& (arr $ const $ liftPrim f)

runStatefulArrow :: (Arrow a) => StatefulArrow a i o -> a i (o,StatefulArrow a i o)
runStatefulArrow (LiftedStatefulArrow lsf) = runStatefulArrow $ liftPrim lsf
runStatefulArrow (StatefulArrow sf) = sf
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
                 returnA -< (o,stateContext sa s')

withState :: (Arrow a,ArrowApply a) => StatefulArrow (StateArrow s a) i o -> s -> StatefulArrow a i o
withState sa s = flip stateContext (sa,s) $
    proc i -> do (StatefulArrow sa',s') <- fetch -< ()
                 ((o,sa''),s'') <- lift app -< (runState sa',(i,s'))
                 store -< (sa'',s'')
                 returnA -< o

withExposedState :: (Arrow a,ArrowApply a) => StatefulArrow (StateArrow s a) i o -> StatefulArrow a (i,s) (o,s)
withExposedState (LiftedStatefulArrow lsa) = LiftedStatefulArrow $ runState lsa
withExposedState (StatefulArrow sa) = StatefulArrow $ (arr $ \((o,sa'),s') -> ((o,s'),withExposedState sa')) <<< runState sa
\end{code}

\subsection{Transforming the underlying type of a StatefulArrow}

\begin{code}
statefulTransform :: (Arrow a,Arrow b) => (forall j p. a j p -> b j p) -> 
                                          StatefulArrow a i o -> StatefulArrow b i o
statefulTransform f (LiftedStatefulArrow lsa) = LiftedStatefulArrow $ f lsa
statefulTransform f (StatefulArrow a) = StatefulArrow $
    proc i -> do (o,a') <- f a -< i
                 returnA -< (o,statefulTransform f a')
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