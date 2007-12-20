\section{The Functional Reactive Programming Arrow: RSAGL.FRP}

Functional Reactive Programming is a paradigm within functional programming.
\footnote{For more information about functional reactive programming in haskell, see http://www.haskell.org/frp/}
In particular, the RSAGL FRP arrow is inspired by YAMPA.
\footnote{http://www.haskell.org/yampa/}
The RSAGL FRP arrow is different from YAMPA in several ways.  Most significantly, it is an arrow transformer.

FRP includes all of the switching and threading operations documented in SwitchedArrow and ThreadedArrow,
and the arrow-embedding operations from FRPBase.

\begin{code}

{-# OPTIONS_GHC -fglasgow-exts -farrows #-}

module RSAGL.FRP
    (FRP,
     RSAGL.FRP.switchContinue,
     RSAGL.FRP.switchTerminate,
     RSAGL.FRP.spawnThreads,
     RSAGL.FRP.killThreadIf,
     RSAGL.FRP.statefulContext,
     frpTest,
     integral,
     derivative,
     absoluteTime,
     threadTime,
     frpContext,
     RSAGL.FRP.withState,
     RSAGL.FRP.withExposedState)
    where

import RSAGL.AbstractVector
import RSAGL.Time
import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.FRPBase as FRPBase
import Data.Monoid
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State

data FRPState = FRPState { frpstate_absolute_time :: Time,
                           frpstate_delta_time :: Time }

newtype FRP i o a j p = FRP (FRPBase i o (StateArrow FRPState a) j p)

instance (Arrow a,ArrowChoice a) => Arrow (FRP i o a) where
    (FRP a) >>> (FRP b) = FRP $ a >>> b
    arr = FRP . arr
    first (FRP f) = FRP $ first f

instance (Arrow a,ArrowChoice a) => ArrowTransformer (FRP i o) a where
    lift = FRP . lift . lift

switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                  FRP i o a (FRP i o a i o,i) o
switchContinue = proc (FRP t,i) -> do FRP $ FRPBase.switchContinue -< (t,i)

switchTerminate :: (Arrow a,ArrowChoice a) =>
                   FRP i o a (FRP i o a i o,o) o
switchTerminate = proc (FRP t,o) -> do FRP $ FRPBase.switchTerminate -< (t,o)

spawnThreads :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => FRP i o a [FRP i o a i o] ()
spawnThreads = proc t -> do FRP $ FRPBase.spawnThreads -< map (\(FRP x) -> x) t

killThreadIf :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => FRP i o a (Bool,o) o
killThreadIf = proc (b,o) -> do FRP $ FRPBase.killThreadIf -< (b,o)
\end{code}

\subsection{Embedding one FRP instance in another}

The frpContext combinator allows a differently-typed FRP thread group to be embedded in another.  
Using frpContext, a thread can instantiate a group of threads that die whenever the calling thread 
dies or switches.  That is, the thread group is part of the state of the calling thread.

\begin{code}
frpContext :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => [FRP j p a j p] -> FRP i o a j p
frpContext = FRP . FRPBase.frpBaseContext . map (\(FRP x) -> x)
\end{code}

\subsection{Embedding a StatefulArrow in an FRP arrow}

\begin{code}
statefulContext :: (Arrow a,ArrowChoice a,ArrowApply a) => StatefulArrow a j p -> FRP i o a j p
statefulContext = FRP . FRPBase.statefulContext . statefulTransform lift

statefulContext_ :: (Arrow a,ArrowChoice a,ArrowApply a) => 
                        StatefulArrow a (j,FRPState) (p,FRPState) -> FRP i o a j p
statefulContext_ sa = proc i ->
    do frpstate <- FRP $ lift fetch -< ()
       (o,frpstate') <- RSAGL.FRP.statefulContext sa -< (i,frpstate)
       FRP $ lift store -< frpstate'
       returnA -< o
\end{code}

\subsection{Allowing underlying state}

\begin{code}
withState :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => 
                 [FRP j p (StateArrow s a) j p] -> s -> FRP i o a j p
withState threads s = statefulContext_ $
    StatefulArrow.withState (RSAGL.FRP.statefulForm threads) s

withExposedState :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) =>
                        [FRP j p (StateArrow s a) j p] -> FRP i o a (j,s) (p,s)
withExposedState threads = RSAGL.FRP.statefulContext_ $
        (arr $ \((i,s),frpstate) -> ((i,frpstate),s))
    >>> (StatefulArrow.withExposedState $ RSAGL.FRP.statefulForm threads)
    >>> (arr $ \((o,frpstate'),s') -> ((o,s'),frpstate'))
\end{code}

\subsection{Invoking an FRP program}

\begin{code}
statefulForm :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) =>
                    [FRP i o a i o] -> StatefulArrow a (i,FRPState) (o,FRPState)
statefulForm = StatefulArrow.withExposedState . FRPBase.statefulForm . map (\(FRP x) -> x)

frpTest :: (Monoid o) => [FRP i o (->) i o] -> [i] -> [o]
frpTest frps is = map fst $ runStateMachine (RSAGL.FRP.statefulForm frps) $
                      zip is frp_test_states

frp_test_states :: [FRPState]
frp_test_states = map numToState [0.0,0.1..]
    where numToState x = FRPState { frpstate_absolute_time = fromSeconds $ 1242341239 + x,
                                    frpstate_delta_time = fromSeconds (if x==0 then 0 else 0.1) }
\end{code}

\subsection{Timelike operations on continuous values.}

integral and derivative respectively take the integral or derivative of a
continuous value.

The integral operation takes an initial value, usually zero.  If the integral
is of a physical or real-world quantity, it will inevitably drift from the "true"
value due to sampling errors. The greater the sampling rate of the FRP, the more 
accurate the result of this operation will be to that "true" value.

These operations are taken in seconds.  For example, if the integral is taken
of a value that is measured in meters per second, then the result will be in meters.
If the derivative is taken of a value that is measured in meters, then the result
will be in meters per second.

\begin{code}
integral :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector av) => av -> FRP i o a (Rate av) av
integral initial_value = statefulContext_ $ SwitchedArrow.withState integral'
                                                (\(i,_) -> (initial_value,i))
    where integral' = proc (new_rate,frpstate@FRPState{ frpstate_delta_time=delta_t }) -> 
              do (old_accum,old_rate) <- lift fetch -< ()
                 let new_accum = old_accum `add` ((scalarMultiply (recip 2) $ new_rate `add` old_rate) `over` delta_t)
                 lift store -< (new_accum,new_rate)
                 returnA -< (new_accum,frpstate)

derivative :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector av) => FRP i o a av (Rate av)
derivative = statefulContext_ $ SwitchedArrow.withState derivative' (\(i,_) -> (i,zero))
    where derivative' = proc (new_value,frpstate@FRPState{ frpstate_delta_time=delta_t }) ->
              do (old_value,old_rate) <- lift fetch -< ()
                 let new_rate = if delta_t == zero
                                then old_rate
                                else (new_value `sub` old_value) `per` delta_t
                 lift store -< (new_value,new_rate)
                 returnA -< (new_rate,frpstate)
\end{code}

\subsection{Getting the time}

absoluteTime answers the time relative to some fixed point (the epoch).

\begin{code}
absoluteTime :: (Arrow a,ArrowChoice a) => FRP i o a () Time
absoluteTime = (arr frpstate_absolute_time) <<< (FRP $ lift fetch)
\end{code}

threadTime answers the time since the current thread first executed.

\begin{code}
threadTime :: (Arrow a,ArrowChoice a,ArrowApply a) => FRP i o a () Time
threadTime = integral zero <<< arr (const $ perSecond $ fromSeconds 1)
\end{code}