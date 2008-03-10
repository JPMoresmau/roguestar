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
     RSAGL.FRP.threadIdentity,
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
     RSAGL.FRP.withExposedState,
     ThreadedArrow.ThreadIdentity,
     ThreadedArrow.nullaryThreadIdentity,
     ThreadedArrow.maybeThreadIdentity,
     ThreadedArrow.unionThreadIdentity,
     whenJust)
    where

import RSAGL.AbstractVector
import RSAGL.Time
import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.ThreadedArrow as ThreadedArrow
import RSAGL.FRPBase as FRPBase
import RSAGL.RK4
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import Data.Maybe

data FRPState = FRPState { frpstate_absolute_time :: Time,
                           frpstate_delta_time :: Time }

data Threaded

type FRP = FRPX Threaded ()
type FRP1 = FRPX () ()

newtype FRPX k t i o a j p = FRP (FRPBase t i o (StateArrow FRPState a) j p)

fromFRP :: FRPX k t i o a j p -> FRPBase t i o (StateArrow FRPState a) j p
fromFRP (FRP frp) = frp

instance (Arrow a,ArrowChoice a) => Arrow (FRPX k t i o a) where
    (FRP a) >>> (FRP b) = FRP $ a >>> b
    arr = FRP . arr
    first (FRP f) = FRP $ first f

instance (Arrow a,ArrowChoice a) => ArrowTransformer (FRPX k t i o) a where
    lift = FRP . lift . lift

instance (ArrowState s a,ArrowChoice a) => ArrowState s (FRPX k t i o a) where
    fetch = lift fetch
    store = lift store

switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                  FRPX any t i o a (Maybe (FRPX any t i o a i o),i) i
switchContinue = proc (s,i) -> FRP $ FRPBase.switchContinue -< (fmap fromFRP s,i)

switchTerminate :: (Arrow a,ArrowChoice a) =>
                   FRPX any t i o a (Maybe (FRPX any t i o a i o),o) o
switchTerminate = proc (s,o) -> FRP $ FRPBase.switchTerminate -< (fmap fromFRP s,o)

spawnThreads :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPX Threaded t i o a [(t,FRPX Threaded t i o a i o)] ()
spawnThreads = proc threads -> FRP $ FRPBase.spawnThreads -< map (second fromFRP) threads

killThreadIf :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPX Threaded t i o a (Bool,o) o
killThreadIf = proc (b,o) -> do FRP $ FRPBase.killThreadIf -< (b,o)

threadIdentity :: (ArrowChoice a) => FRPX any t i o a () t
threadIdentity = FRP $ FRPBase.threadIdentity
\end{code}

\subsection{Embedding one FRP instance in another}

The frpContext combinator allows a differently-typed FRP thread group to be embedded in another.  
Using frpContext, a thread can instantiate a group of threads that die whenever the calling thread 
dies or switches.  That is, the thread group is part of the state of the calling thread.

\begin{code}
frpContext :: (Arrow a,ArrowChoice a,ArrowApply a) => 
    (forall x. j -> [(t,x)] -> [(t,(p,x))] -> [x]) -> 
    [(t,FRPX Threaded t j p a j p)] -> FRPX any u i o a j [(t,p)]
frpContext manageThreads = FRP . FRPBase.frpBaseContext manageThreads . map (second fromFRP)

frp1Context :: (Arrow a,ArrowChoice a,ArrowApply a) => 
    FRP1 j p a j p -> FRPX any t i o a j p
frp1Context (FRP frp1_thread) = arr (fromSingletonList (error "frp1Context: non-singular list") . map snd) <<< 
                                frpContext ThreadedArrow.nullaryThreadIdentity [((),FRP frp1_thread)]

fromSingletonList :: ([x] -> x) -> [x] -> x
fromSingletonList nonsingleF list = case list of
                               [x] -> x
			       xs -> nonsingleF xs

\end{code}

\subsection{Embedding a StatefulArrow in an FRP arrow}

\begin{code}
statefulContext :: (Arrow a,ArrowChoice a,ArrowApply a) => StatefulArrow a j p -> FRPX any t i o a j p
statefulContext = FRP . FRPBase.statefulContext . statefulTransform lift

statefulContext_ :: (Arrow a,ArrowChoice a,ArrowApply a) => 
                        StatefulArrow a (j,FRPState) (p,FRPState) -> FRPX any t i o a j p
statefulContext_ sa = proc i ->
    do frpstate <- FRP (lift fetch) -< ()
       (o,frpstate') <- RSAGL.FRP.statefulContext sa -< (i,frpstate)
       FRP (lift store) -< frpstate'
       returnA -< o
\end{code}

\subsection{Allowing underlying state}

\begin{code}
withState :: (Arrow a,ArrowChoice a,ArrowApply a) => 
   (forall x. j -> [(t,x)] -> [(t,(p,x))] -> [x]) ->               
   [(t,FRPX Threaded t j p (StateArrow s a) j p)] -> s -> FRPX any u i o a j [(t,p)]
withState manageThreads threads s = statefulContext_ $
    StatefulArrow.withState (RSAGL.FRP.statefulForm manageThreads threads) s

withExposedState :: (Arrow a,ArrowChoice a,ArrowApply a) =>
    (forall x. j -> [(t,x)] -> [(t,(p,x))] -> [x]) ->
    [(t,FRPX Threaded t j p (StateArrow s a) j p)] -> FRPX any u i o a (j,s) ([(t,p)],s)
withExposedState manageThreads threads = RSAGL.FRP.statefulContext_ $
        (arr $ \((i,s),frpstate) -> ((i,frpstate),s))
    >>> (StatefulArrow.withExposedState $ RSAGL.FRP.statefulForm manageThreads threads)
    >>> (arr $ \((o,frpstate'),s') -> ((o,s'),frpstate'))
\end{code}

\subsection{Invoking an FRP program}

\begin{code}
statefulForm :: (Arrow a,ArrowChoice a,ArrowApply a) =>
    (forall x. i -> [(t,x)] -> [(t,(o,x))] -> [x]) ->
    [(t,FRPX Threaded t i o a i o)] -> StatefulArrow a (i,FRPState) ([(t,o)],FRPState)
statefulForm manageThreads = StatefulArrow.withExposedState . FRPBase.statefulForm manageThreads . map (second fromFRP)

frpTest :: [FRP i o (->) i o] -> [i] -> [[o]]
frpTest frps is = map (map snd . fst) $ runStateMachine (RSAGL.FRP.statefulForm nullaryThreadIdentity $ (map $ \x -> ((),x)) frps) $
                      zip is frp_test_states

frp_test_states :: [FRPState]
frp_test_states = map numToState [0.0,0.1..]
    where numToState x = FRPState { frpstate_absolute_time = fromSeconds $ 1242341239 + x,
                                    frpstate_delta_time = fromSeconds (if x==0 then 0 else 0.1) }

data FRPProgram a i o = FRPProgram (StatefulArrow a (i,FRPState) ([o],FRPState)) (Maybe Time)

newFRPProgram :: (Arrow a,ArrowChoice a,ArrowApply a) => 
    (forall x. i -> [(t,x)] -> [(t,(o,x))] -> [x]) ->
    [(t,FRPX Threaded t i o a i o)] -> FRPProgram a i [(t,o)]
newFRPProgram manageThreads frps = newFRP1Program (frpContext manageThreads frps)

newFRP1Program :: (Arrow a,ArrowChoice a,ArrowApply a) => FRP1 i o a i o -> FRPProgram a i o
newFRP1Program frp1 = FRPProgram (RSAGL.FRP.statefulForm nullaryThreadIdentity [((),frp1Context frp1)] >>> arr (first $ map snd)) Nothing

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
derivative :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractSubtract p v,AbstractVector v) => FRPX any t i o a p (Rate v)
derivative = accumulate (error "derivative: undefined")
    (\old_value new_value old_rate _ delta_t _ -> if delta_t == zero
        then old_rate
        else (new_value `sub` old_value) `per` delta_t)
    zero

integral :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector v) => v -> FRPX any t i o a (Rate v) v
integral = accumulate (error "integral: undefined")
    (\old_rate new_rate old_accum _ delta_t _ -> old_accum `add` ((scalarMultiply (recip 2) $ new_rate `add` old_rate) `over` delta_t))

integralRK4 :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector v) => Frequency -> (p -> v -> p) -> p -> FRPX any t i o a (Time -> p -> Rate v) p
integralRK4 f addPV = accumulate f (\_ diffF p abs_t delta_t -> integrateRK4 addPV diffF p (abs_t `sub` delta_t) abs_t)

integralRK4' :: (Arrow a,ArrowChoice a,ArrowApply a,AbstractVector v) => Frequency -> (p -> v -> p) -> (p,Rate v) -> 
                FRPX any t x y a (Time -> p -> Rate v -> Acceleration v) (p,Rate v)
integralRK4' f addPV = accumulate f (\_ diffF p abs_t delta_t -> integrateRK4' addPV diffF p (abs_t `sub` delta_t) abs_t)
\end{code}

The \texttt{accumulate} function implements practically any time-stepping algorithm as though it were continuous.
The accumulation function is of the form: old\_input new\_input old\_accumulation absolute\_time delta\_time number\_of\_intervals.

\begin{code}
accumulate :: (Arrow a,ArrowChoice a,ArrowApply a) => Frequency -> (i -> i -> o -> Time -> Time -> Integer -> o) -> o -> FRPX any t x y a i o
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
absoluteTime :: (Arrow a,ArrowChoice a) => FRPX any t i o a () Time
absoluteTime = (arr frpstate_absolute_time) <<< FRP (lift fetch)
\end{code}

threadTime answers the time since the current thread first executed.

\begin{code}
threadTime :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPX any t i o a () Time
threadTime = integral zero <<< arr (const $ perSecond $ fromSeconds 1)
\end{code}

\subsection{Additional Combinators}

\texttt{whenJust} permits some computations on \texttt{Maybe} values, but the implcit state of
the computation is reset whenever \texttt{whenJust} recieves \texttt{Nothing}.  If this is not
your intention, you might use \texttt{whenJust <<< sticky isJust Nothing}.

\begin{code}
whenJust :: (ArrowChoice a,ArrowApply a) => (forall x y. FRP1 x y a j p) -> FRPX any t i o a (Maybe j) (Maybe p)
whenJust actionA = frp1Context whenJust_
    where whenJust_ = proc i ->
              do RSAGL.FRP.switchContinue -< (maybe (Just whenNothing_) (const Nothing) i,i)
	         arr (Just) <<< actionA -< fromMaybe (error "whenJust: impossible case") i
	  whenNothing_ = proc i ->
	      do RSAGL.FRP.switchContinue -< (fmap (const whenJust_) i,i)
	         returnA -< Nothing
\end{code}
