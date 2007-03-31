\section{RSAGL.FRPBase}

The FRPBase arrow is a composite of the Stateful and Threaded arrows intended to support functional reactive programming.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts -farrows #-}

module RSAGL.FRPBase
    (FRPBase,
     statefulFRPBase)
    where

import Data.Monoid
import qualified Data.Set as Set
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.ThreadedArrow as ThreadedArrow
import RSAGL.Time
\end{code}

FRPBase is a composite arrow in which the StatefulArrow is layered on top of the ThreadedArrow.

\begin{code}
newtype FRPBase i o a j p = FRPBase (StatefulArrow (ThreadedArrow i o a) j p)

instance (ArrowChoice a) => Arrow (FRPBase i o a) where
    (>>>) (FRPBase x) (FRPBase y) = FRPBase $ x >>> y
    arr = FRPBase . arr
    first (FRPBase f) = FRPBase $ first f

instance (ArrowChoice a) => ArrowTransformer (FRPBase i o) a where
    lift = FRPBase . lift . lift

switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPBase i o a (FRPBase i o a i o,i) o
switchContinue = 
    proc (FRPBase thread,i) -> 
        do FRPBase $ lift $ ThreadedArrow.switchContinue -< (statefulThread thread,i)

switchTerminate :: (Arrow a,ArrowChoice a) => FRPBase i o a (FRPBase i o a i o,o) o
switchTerminate = 
    proc (FRPBase thread,o) -> 
        do FRPBase $ lift $ ThreadedArrow.switchTerminate -< (statefulThread thread,o)

spawnThreads :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPBase i o a [FRPBase i o a i o] ()
spawnThreads = (FRPBase $ lift $ ThreadedArrow.spawnThreads) <<< 
               arr (map (\(FRPBase thread) -> statefulThread thread))

killThreadIf :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => FRPBase i o a (Bool,o) o
killThreadIf = FRPBase $ lift ThreadedArrow.killThreadIf
\end{code}

\subsection{Embedding one FRPBase instance in another}

The frpContext combinator allows a differently-typed FRPBase to be embedded in another.  
Using frpContext, a thread can instantiate a group of threads that die whenever the calling thread 
dies or switches.  That is, the thread group is part of the state of the calling thread.

\begin{code}
frpContext :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => [FRPBase j p a j p] -> FRPBase i o a j p
frpContext threads = FRPBase $ statefulTransform lift (statefulFRPBase threads)
\end{code}

\subsection{Embedding explicit underlying state}

The stateContext combinator allows an FRPBase arrow to be embedded in another, with added state
information.  

\begin{code}
stateContext :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => 
                [FRPBase j p (StateArrow s a) j p] -> s -> FRPBase i o a j p
stateContext threads s = FRPBase $ statefulTransform lift $ underlyingState (statefulFRPBase threads) s
\end{code}

\subsection{Embedding the FRP arrow as a StatefulArrow}

The FRPBase arrow can be made to appear as a StatefulArrow using statefulFRP.

\begin{code}
statefulFRPBase :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => [FRPBase i o a i o] -> StatefulArrow a i o
statefulFRPBase = threadedContext . map (\(FRPBase x) -> statefulThread x)
\end{code}

\subsection{Underlying mechanism}

The underlying mechanism of the FRPBase arrow is the layering of a StatefulArrow over a ThreadedArrow,
allowing both implicitly and explicitly stateful functions.  statefulThread implements this.

\begin{code}
statefulThread :: (Arrow a,ArrowChoice a) => 
                  StatefulArrow (ThreadedArrow i o a) i o -> ThreadedArrow i o a i o
statefulThread (StatefulArrow sf) = 
    proc b -> do (c,sf') <- sf -< b
                 ThreadedArrow.switchTerminate -< (statefulThread sf',c)
\end{code}