\section{RSAGL.FRPBase}

The FRPBase arrow is a composite of the Stateful and Threaded arrows intended to support functional reactive programming.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts -farrows #-}

module RSAGL.FRPBase
    (FRPBase,
     RSAGL.FRPBase.switchContinue,
     RSAGL.FRPBase.switchTerminate,
     RSAGL.FRPBase.spawnThreads,
     RSAGL.FRPBase.killThreadIf,
     RSAGL.FRPBase.frpBaseContext,
     RSAGL.FRPBase.withState,
     RSAGL.FRPBase.withExposedState,
     RSAGL.FRPBase.statefulContext,
     RSAGL.FRPBase.statefulForm)
    where

import Data.Monoid
import Control.Arrow
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.StatefulArrow as StatefulArrow
import RSAGL.ThreadedArrow as ThreadedArrow
\end{code}

FRPBase is a composite arrow in which the StatefulArrow is layered on top of the ThreadedArrow.

\begin{code}
newtype FRPBase i o a j p = FRPBase (StatefulArrow (ThreadedArrow i o a) j p)

fromFRPBase :: FRPBase i o a j p -> (StatefulArrow (ThreadedArrow i o a) j p)
fromFRPBase (FRPBase a) = a

instance (ArrowChoice a) => Arrow (FRPBase i o a) where
    (>>>) (FRPBase x) (FRPBase y) = FRPBase $ x >>> y
    arr = FRPBase . arr
    first (FRPBase f) = FRPBase $ first f

instance (ArrowChoice a) => ArrowTransformer (FRPBase i o) a where
    lift = FRPBase . lift . lift

switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPBase i o a (Maybe (FRPBase i o a i o),i) i
switchContinue = proc (thread,i) -> 
    do FRPBase $ lift $ ThreadedArrow.switchContinue -< (fmap (statefulThread . fromFRPBase) thread,i)

switchTerminate :: (Arrow a,ArrowChoice a) => FRPBase i o a (Maybe (FRPBase i o a i o),o) o
switchTerminate = proc (thread,o) -> 
    do FRPBase $ lift $ ThreadedArrow.switchTerminate -< (fmap (statefulThread . fromFRPBase) thread,o)

spawnThreads :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPBase i o a [FRPBase i o a i o] ()
spawnThreads = (FRPBase $ lift $ ThreadedArrow.spawnThreads) <<< 
               arr (map (\(FRPBase thread) -> statefulThread thread))

killThreadIf :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid o) => FRPBase i o a (Bool,o) o
killThreadIf = FRPBase $ lift ThreadedArrow.killThreadIf
\end{code}

\subsection{Embedding one FRPBase instance in another}

The frpBaseContext combinator allows a differently-typed FRPBase thread group to be embedded in another.  
Using frpBaseContext, a thread can instantiate a group of threads that die whenever the calling thread 
dies or switches.  That is, the thread group is part of the state of the calling thread.

\begin{code}
frpBaseContext :: (Arrow a,ArrowChoice a,ArrowApply a) => [FRPBase j p a j p] -> FRPBase i o a j [p]
frpBaseContext threads = FRPBase $ statefulTransform lift (RSAGL.FRPBase.statefulForm threads)
\end{code}

\subsection{Embedding explicit underlying state}

The withState and withExposedState combinators are implemented in terms of the
StatefulArrow combinators of the same name\footnote{See page \pageref{withState}}.

\begin{code}
withState :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) => 
                [FRPBase j p (StateArrow s a) j p] -> s -> FRPBase i o a j [p]
withState threads s = FRPBase $ statefulTransform lift $ StatefulArrow.withState (RSAGL.FRPBase.statefulForm threads) s

withExposedState :: (Arrow a,ArrowChoice a,ArrowApply a,Monoid p) =>
                [FRPBase j p (StateArrow s a) j p] -> FRPBase i o a (j,s) ([p],s)
withExposedState threads = statefulContext $ StatefulArrow.withExposedState $ RSAGL.FRPBase.statefulForm threads
\end{code}

\subsection{Embedding a StatefulArrow in an FRPBase arrow}
\label{statefulContext}

The statefulContext combinator allows a StatefulArrow to be embedded in an FRPBase, with
the provision that the StatefulArrow does not have access to the threading operators.

\begin{code}
statefulContext :: (Arrow a,ArrowChoice a,ArrowApply a) =>
                       StatefulArrow a j p -> FRPBase i o a j p
statefulContext = FRPBase . statefulTransform lift
\end{code}

\subsection{The StatefulArrow form of an FRPBase arrow}

The FRPBase arrow can be made to appear as a StatefulArrow using statefulForm.
\footnote{See \pageref{statefulForm}}

\begin{code}
statefulForm :: (Arrow a,ArrowChoice a,ArrowApply a) => [FRPBase i o a i o] -> StatefulArrow a i [o]
statefulForm = ThreadedArrow.statefulForm . map (\(FRPBase x) -> statefulThread x)
\end{code}

\subsection{Essential mechanism}

The essential mechanism of the FRPBase arrow is the layering of a StatefulArrow over a ThreadedArrow,
allowing threads that are both implicitly and explicitly stateful.  \texttt{statefulThread} implements this.
In the event of an explicit switch, all implicit state is lost.

\begin{code}
statefulThread :: (Arrow a,ArrowChoice a) => 
                  StatefulArrow (ThreadedArrow i o a) i o -> ThreadedArrow i o a i o
statefulThread (LiftedStatefulArrow lsf) = lsf
statefulThread (StatefulArrow sf) = 
    proc b -> do (c,sf') <- sf -< b
                 ThreadedArrow.switchTerminate -< (Just $ statefulThread sf',c)
\end{code}
