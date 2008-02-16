\section{The Functional Reactive Programming Arrow: RSAGL.FRP}

Functional Reactive Programming is a paradigm within functional programming.
\footnote{For more information about functional reactive programming in haskell, see http://www.haskell.org/frp/}
In particular, the RSAGL FRP arrow is inspired by YAMPA.
\footnote{http://www.haskell.org/yampa/}
The RSAGL FRP arrow is different from YAMPA in several ways.  Most significantly, it is an arrow transformer.

FRP includes all of the switching and threading operations documented in SwitchedArrow and ThreadedArrow,
and the arrow-embedding operations from FRPBase.

\begin{code}

{-# OPTIONS_GHC -fglasgow-exts -farrows -fallow-undecidable-instances #-}

module RSAGL.FRP
    (FRPX,
     FRP1,
     Threaded,
     FRP,
     RSAGL.FRP.switchContinue,
     RSAGL.FRP.switchTerminate,
     RSAGL.FRP.spawnThreads,
     RSAGL.FRP.killThreadIf,
     RSAGL.FRP.statefulContext,
     frpTest,
     FRPProgram,
     newFRPProgram,
     newFRP1Program,
     updateFRPProgram,
     integral,
     derivative,
     integralRK4,
     integralRK4',
     absoluteTime,
     threadTime,
     frpContext,
     frp1Context,
     RSAGL.FRP.withState,
     RSAGL.FRP.withExposedState)
    where

import RSAGL.ArrowTransformerOptimizer
import RSAGL.AbstractVector
import RSAGL.Time
import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.FRPBase as FRPBase
import RSAGL.RK4
import Data.Monoid
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import Data.Maybe

data FRPState = FRPState { frpstate_absolute_time :: Time,
                           frpstate_delta_time :: Time }

data Threaded

type FRP = FRPX Threaded
type FRP1 = FRPX ()

newtype FRPX k i o a j p = FRP (ArrowTransformerOptimizer (FRPBase i o) (StateArrow FRPState a) j p)

fromFRP :: FRPX k i o a j p -> (FRPBase i o (StateArrow FRPState a) j p)
fromFRP (FRP frp) = collapseArrowTransformer frp

instance (Arrow a,ArrowChoice a) => Arrow (FRPX k i o a) where
    (FRP a) >>> (FRP b) = FRP $ a >>> b
    arr = FRP . arr
    first (FRP f) = FRP $ first f

instance (Arrow a,ArrowChoice a) => ArrowTransformer (FRPX k i o) a where
    lift = FRP . lift . lift

instance (ArrowState s a,ArrowChoice a) => ArrowState s (FRPX k i o a) where
    fetch = lift fetch
    store = lift store

switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                  FRPX any i o a (Maybe (FRPX any i o a i o),i) i
switchContinue = proc (t,i) -> FRP $ raw $ FRPBase.switchContinue -< (fmap fromFRP t,i)

switchTerminate :: (Arrow a,ArrowChoice a) =>
                   FRPX any i o a (Maybe (FRPX any i o a i o),o) o
switchTerminate = proc (t,o) -> FRP $ raw $ FRPBase.switchTerminate -< (fmap fromFRP t,o)

spawnThreads :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => FRP i o a [FRP i o a i o] ()
spawnThreads = proc t -> FRP $ raw $ FRPBase.spawnThreads -< map fromFRP t

killThreadIf :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => FRP i o a (Bool,o) o
killThreadIf = proc (b,o) -> do FRP $ raw $ FRPBase.killThreadIf -< (b,o)
\end{code}

\subsection{Embedding one FRP instance in another}

The frpContext combinator allows a differently-typed FRP thread group to be embedded in another.  
Using frpContext, a thread can instantiate a group of threads that die whenever the calling thread 
dies or switches.  That is, the thread group is part of the state of the calling thread.

\begin{code}
frpContext :: (Arrow a,ArrowChoice a,ArrowApply a) => [FRP j p a j p] -> FRPX any i o a j [p]
frpContext = FRP . raw . FRPBase.frpBaseContext . map (\(FRP x) -> collapseArrowTransformer x)

frp1Context :: (Arrow a,ArrowChoice a,ArrowApply a) => FRP1 j p a j p -> FRPX any i o a j p
frp1Context (FRP frp1_thread) = arr (fromSingletonList (error "frp1Context: non-singular list")) <<< frpContext [FRP frp1_thread]

fromSingletonList :: ([x] -> x) -> [x] -> x
fromSingletonList nonsingleF list = case list of
                               [x] -> x
			       xs -> nonsingleF xs

\end{code}

\subsection{Embedding a StatefulArrow in an FRP arrow}

\begin{code}
statefulContext :: (Arrow a,ArrowChoice a,ArrowApply a) => StatefulArrow a j p -> FRPX any i o a j p
statefulContext = FRP . raw . FRPBase.statefulContext . statefulTransform lift

statefulContext_ :: (Arrow a,ArrowChoice a,ArrowApply a) => 
                        StatefulArrow a (j,FRPState) (p,FRPState) -> FRPX any i o a j p
statefulContext_ sa = proc i ->
    do frpstate <- FRP (lift fetch) -< ()
       (o,frpstate') <- RSAGL.FRP.statefulContext sa -< (i,frpstate)
       FRP (lift store) -< frpstate'
       returnA -< o
\end{code}

\subsection{Allowing underlying state}

\begin{code}
withState :: (Arrow a,ArrowChoice a,ArrowApply a) => 
                 [FRP j p (StateArrow s a) j p] -> s -> FRPX any i o a j [p]
withState threads s = statefulContext_ $
    StatefulArrow.withState (RSAGL.FRP.statefulForm threads) s

withExposedState :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                        [FRP j p (StateArrow s a) j p] -> FRPX any i o a (j,s) ([p],s)
withExposedState threads = RSAGL.FRP.statefulContext_ $
        (arr $ \((i,s),frpstate) -> ((i,frpstate),s))
    >>> (StatefulArrow.withExposedState $ RSAGL.FRP.statefulForm threads)
    >>> (arr $ \((o,frpstate'),s') -> ((o,s'),frpstate'))
\end{code}

\subsection{Invoking an FRP program}

\begin{code}
statefulForm :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                    [FRP i o a i o] -> StatefulArrow a (i,FRPState) ([o],FRPState)
statefulForm = StatefulArrow.withExposedState . FRPBase.statefulForm . map (\(FRP x) -> collapseArrowTransformer x)

frpTest :: [FRP i o (->) i o] -> [i] -> [[o]]
frpTest frps is = map fst $ runStateMachine (RSAGL.FRP.statefulForm frps) $
                      zip is frp_test_states

frp_test_states :: [FRPState]
frp_test_states = map numToState [0.0,0.1..]
    where numToState x = FRPState { frpstate_absolute_time = fromSeconds $ 1242341239 + x,
                                    frpstate_delta_time = fromSeconds (if x==0 then 0 else 0.1) }

data FRPProgram a i o = FRPProgram (StatefulArrow a (i,FRPState) ([o],FRPState)) (Maybe Time)

newFRPProgram :: (Arrow a,ArrowChoice a,ArrowApply a) => [FRP i o a i o] -> FRPProgram a i [o]
newFRPProgram frps = newFRP1Program (frpContext frps)

newFRP1Program :: (Arrow a,ArrowChoice a,ArrowApply a) => FRP1 i o a i o -> FRPProgram a i o
newFRP1Program frp1 = FRPProgram (RSAGL.FRP.statefulForm [frp1Context frp1]) Nothing

updateFRPProgram :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPProgram a i o -> a (i,Time) (o,FRPProgram a i o)
updateFRPProgram (FRPProgram sa old_run) = proc (i,new_t) ->
    do let old_t = fromMaybe new_t old_run
       ((new_os,_),new_stateful_arrow) <- runStatefulArrow sa -< (i, FRPState { frpstate_absolute_time = new_t, frpstate_delta_time = (new_t `sub` old_t) })
       returnA -< (fromSingletonList (error "updateFRPProgram: non-singular list") new_os, FRPProgram new_stateful_arrow (Just new_t))
\end{code}

\subsection{Timelike operations on continuous values.}

\texttt{integral} and \texttt{derivative} respectively take the integral or derivative of a
continuous value.

\begin{code}
derivative :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractSubtract p v,AbstractVector v) => FRPX any i o a p (Rate v)
derivative = accumulate undefined
    (\old_value new_value old_rate _ delta_t _ -> if delta_t == zero
        then old_rate
        else (new_value `sub` old_value) `per` delta_t)
    zero

integral :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector v) => v -> FRPX any i o a (Rate v) v
integral = accumulate undefined
    (\old_rate new_rate old_accum _ delta_t _ -> old_accum `add` ((scalarMultiply (recip 2) $ new_rate `add` old_rate) `over` delta_t))

integralRK4 :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector v) => Frequency -> (p -> v -> p) -> p -> FRPX any i o a (Time -> p -> Rate v) p
integralRK4 f addPV = accumulate f (\_ diffF p abs_t delta_t -> integrateRK4 addPV diffF p (abs_t `sub` delta_t) abs_t)

integralRK4' :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector v) => Frequency -> (p -> v -> p) -> (p,Rate v) -> 
                FRPX any x y a (Time -> p -> Rate v -> Acceleration v) (p,Rate v)
integralRK4' f addPV = accumulate f (\_ diffF p abs_t delta_t -> integrateRK4' addPV diffF p (abs_t `sub` delta_t) abs_t)
\end{code}

The \texttt{accumulate} function implements practically any time-stepping algorithm as though it were continuous.
The accumulation function is of the form: old\_input new\_input old\_accumulation absolute\_time delta\_time number\_of\_intervals.

\begin{code}
accumulate :: (Arrow a,ArrowChoice a,ArrowApply a) => Frequency -> (i -> i -> o -> Time -> Time -> Integer -> o) -> o -> FRPX any x y a i o
accumulate frequency accumF initial_value = statefulContext_ $ SwitchedArrow.withState accumulate_ (\(i,_) -> (i,initial_value))
    where accumulate_ = proc (new_input,frpstate@FRPState{ frpstate_absolute_time=abs_t, frpstate_delta_time=delta_t }) -> 
              do (old_input,old_accum) <- lift fetch -< ()
                 let new_accum = accumF old_input new_input old_accum abs_t delta_t (ceiling $ toSeconds delta_t / toSeconds (interval frequency))
                 lift store -< (new_input,new_accum)
                 returnA -< (new_accum,frpstate)
\end{code}

\subsection{Getting the time}

absoluteTime answers the time relative to some fixed point (the epoch).

\begin{code}
absoluteTime :: (Arrow a,ArrowChoice a) => FRPX any i o a () Time
absoluteTime = (arr frpstate_absolute_time) <<< FRP (lift fetch)
\end{code}

threadTime answers the time since the current thread first executed.

\begin{code}
threadTime :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPX any i o a () Time
threadTime = integral zero <<< arr (const $ perSecond $ fromSeconds 1)
\end{code}
