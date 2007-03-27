\section{RSAGL.FRP, the Functional Reactive Programming Arrow}

Functional Reactive Programming is a paradigm within functional programming.
\footnote{For more information about functional reactive programming in haskell, see http://www.haskell.org/frp/}
In particular, the RSAGL FRP arrow is inspired by YAMPA.
\footnote{See Arrows, Robots, and Functional Reactive Programming: http://www.haskell.org/yampa/AFPLectureNotes.pdf}
The RSAGL FRP arrow is different from YAMPA in several ways.  Most significantly, it is an arrow transformer.

FRP does not support ArrowChoice or ArrowApply, otherwise the signal functions (such as the edge detector) could 
be deprived of input and, as a result, provide erroneous output.  However, the underlying arrow must provide these.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts -farrows #-}

module RSAGL.FRP
    (FRP,
     statefulFRP)
    where

import Data.Monoid
import qualified Data.Set as Set
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.StatefulArrow
import RSAGL.SwitchedArrow
import RSAGL.ThreadedArrow
import RSAGL.Time
\end{code}

A FRPThread is an FRP arrow of its own type, and can be passed to switchContinue, switchTerminate, or spawnThreads.
A FRP arrow that does not match its own type is only a stage in the computation, and can't be used
as a switch or thread.
\footnote{For a simpler example see the documentation for Switch as a subset of the types of SwitchedArrows. (page \pageref{Switch})}

\begin{code}
type FRPThread a i o = FRP i o a i o
newtype FRP i o a j p = FRP (StatefulArrow (ThreadedArrow i o (StateArrow (FRPState a i o) a)) j p)

data FRPState a i o = FRPState { frpstate_absolute_time :: Time,
                                 frpstate_delta_time :: Time,
                                 frpstate_switch :: StatefulArrow (StateArrow (FRPState a i o) a) i o }

instance (ArrowChoice a) => Arrow (FRP i o a) where
    (>>>) (FRP x) (FRP y) = FRP $ x >>> y
    arr = FRP . arr
    first (FRP f) = FRP $ first f

instance (ArrowChoice a) => ArrowTransformer (FRP i o) a where
    lift = FRP . lift . lift . lift

instance ArrowSwitched FRP where
    switchContinue = proc (FRP thread,i) -> do FRP $ lift $ switchContinue -< (statefulThread thread,i)
    switchTerminate = proc (FRP thread,o) -> do FRP $ lift $ switchTerminate -< (statefulThread thread,o)

instance ArrowThreaded FRP where
    spawnThreads = (FRP $ lift $ spawnThreads) <<< arr (map (\(FRP thread) -> statefulThread thread))
    killThread = FRP $ lift killThread

absoluteTime :: (Arrow a,ArrowChoice a) => FRP i o a () Time
absoluteTime = arr frpstate_absolute_time <<< (FRP $ lift $ lift $ fetch)
\end{code}

\subsection{Embedding the FRP arrow as a StatefulArrow}

The FRP arrow can be made to appear as a StatefulArrow using statefulFRP.  Since the FRP arrow needs to
know the time at the beginning of each iteration, it must be the responsibility of the caller to provide
that information.  Thus, the type of the statefulFRP's result arrow is amended to include a Time parameter.

\begin{code}
statefulFRP :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => [FRPThread a i o] -> StatefulArrow a (i,Time) o
statefulFRP threads = 
    let initial_state = FRPState { frpstate_absolute_time = undefined,
                                   frpstate_delta_time = 0,
                                   frpstate_switch = makeStateful threads }
        firstpass cont = StatefulArrow $ -- intercept and set the absolute time on the first pass
            proc (i,t) -> do app -< (runStatefulArrow $ 
                                        cont (initial_state { frpstate_absolute_time = t }),(i,t))
        in firstpass $ RSAGL.StatefulArrow.stateContext $
            proc (i,t) -> do context <- fetch -< ()
                             store -< context { frpstate_absolute_time = t,
                                                frpstate_delta_time = t - frpstate_absolute_time context }
                             (o,next) <- app -< (runStatefulArrow $ frpstate_switch context,i)
                             store -< context { frpstate_switch = next }
                             returnA -< o

makeStateful :: (ArrowChoice a,ArrowApply a,Monoid o) => 
                [FRPThread a i o] -> StatefulArrow (StateArrow (FRPState a i o) a) i o
makeStateful = threadedContext . map (\(FRP thread) -> statefulThread thread)

statefulThread :: (Arrow a,ArrowChoice a) => 
                  StatefulArrow (ThreadedArrow i o a) i o -> ThreadedArrow i o a i o
statefulThread (StatefulArrow sf) = 
    proc b -> do (c,sf') <- sf -< b
                 switchTerminate -< (statefulThread sf',c)
\end{code}

\subsection{Embedding one FRP arrow in another}

frpContext allows a differently-typed FRP arrow to be embedded in another FRP arrow.  Using frpContext,
an FRP thread can spawn a group of threads that die whenever the calling thread dies or switches.
That is, the thread group is part of the state of the calling thread.

\begin{code}
frpContext :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => [FRPThread a j p] -> FRP i o a j p
frpContext threads = FRP $ frp_ $ statefulFRP threads
                         where frp_ f = StatefulArrow $ lift $ 
                                            proc i -> do t <- arr frpstate_absolute_time <<< fetch -< ()
                                                         (o,next) <- lift $ runStatefulArrow f -< (i,t)
                                                         returnA -< (o,frp_ next)
\end{code}