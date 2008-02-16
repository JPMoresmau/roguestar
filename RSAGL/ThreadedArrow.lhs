\section{RSAGL.ThreadedArrow}

A ThreadedArrow is an extension of the SwitchedArrow.  In addition to switching, ThreadedArrow threads can spawn new threads and
terminate themselves.  All threads recieve the same input.

While the ThreadedArrow has conceptual similarities to the threading systems provided by an operating system,
it does not provide true parallelism or concurency.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.ThreadedArrow
    (ThreadedFunction,
     ThreadedArrow,
     RSAGL.ThreadedArrow.switchContinue,
     RSAGL.ThreadedArrow.switchTerminate,
     spawnThreads,
     killThreadIf,
     RSAGL.ThreadedArrow.statefulForm)
    where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.StatefulArrow as StatefulArrow
import Data.Maybe
\end{code}

The ThreadedArrow itself is a form of SwitchedArrow.  It retains state (ThreadInfo) that
indicates what threads have completed, are waiting to be completed, and are currently executing.

\begin{code}
type ThreadedFunction i o = ThreadedArrow i o (->)
data ThreadInfo a i o = ThreadInfo { ti_active_thread :: Maybe (ThreadedArrow i o a i o),
                                     ti_waiting_threads :: [ThreadedArrow i o a i o],
                                     ti_completed_threads :: [ThreadedArrow i o a i o] }

initialThreadedState :: [ThreadedArrow i o a i o] -> ThreadInfo a i o
initialThreadedState threads = ThreadInfo { ti_active_thread = Nothing,
                                            ti_waiting_threads = threads,
                                            ti_completed_threads = [] }

newtype ThreadedArrow i o a j p = ThreadedArrow (SwitchedArrow i o (StateArrow (ThreadInfo a i o) a) j p)

instance (ArrowChoice a) => Arrow (ThreadedArrow i o a) where
    (>>>) (ThreadedArrow ta1) (ThreadedArrow ta2) = ThreadedArrow $ ta1 >>> ta2
    arr = ThreadedArrow . arr
    first (ThreadedArrow f) = ThreadedArrow $ first f

instance (Arrow a,ArrowChoice a) => ArrowTransformer (ThreadedArrow i o) a where
    lift = ThreadedArrow . lift . lift

instance (ArrowChoice a) => ArrowChoice (ThreadedArrow i o a) where
    left (ThreadedArrow a) = ThreadedArrow $ left a

instance (ArrowChoice a,ArrowApply a) => ArrowApply (ThreadedArrow i o a) where
    app = ThreadedArrow $ proc (ThreadedArrow a,b) -> app -< (a,b)

loadNextThread :: (Arrow a) => StateArrow (ThreadInfo a i o) a () (ThreadInfo a i o)
loadNextThread = fetch >>> arr loadNextThread_ >>> store >>> fetch
    where loadNextThread_ thread_info =
              case thread_info of
                  t@(ThreadInfo { ti_active_thread = Nothing, ti_waiting_threads = (thread:threads) }) -> 
                           (t { ti_active_thread = Just thread,
                                ti_waiting_threads = threads })
                  t -> t

storeCurrentThread :: (Arrow a) => StateArrow (ThreadInfo a i o) a () ()
storeCurrentThread = fetch >>> arr storeCurrentThread_ >>> store
    where storeCurrentThread_ thread_info =
              thread_info { ti_completed_threads = maybeToList (ti_active_thread thread_info) ++ 
                                                   ti_completed_threads thread_info,
                            ti_active_thread = Nothing }

runThread :: (Arrow a,ArrowChoice a) => ThreadedArrow i o a i o -> StateArrow (ThreadInfo a i o) a i o
runThread (ThreadedArrow thread) = 
    let (StatefulArrow switched_form) = SwitchedArrow.statefulForm thread
        in proc i -> do (o,_) <- switched_form -< i
                        storeCurrentThread -< ()
                        returnA -< o

runThreads :: (Arrow a,ArrowApply a,ArrowChoice a) => StateArrow (ThreadInfo a i o) a i [o]
runThreads = proc i ->
    do thread_info <- loadNextThread -< ()
       case ti_active_thread thread_info of
           Just thread -> do o <- app -< (runThread thread,i)
                             os <- runThreads -< i
                             returnA -< o : os
           Nothing -> returnA -< []
\end{code}

The ThreadedArrow implements the same switching semantics as SwitchedArrow.
\footnote{Page \pageref{switchContinue}}

\begin{code}
switchContinue :: (Arrow a,ArrowChoice a,ArrowApply a) => ThreadedArrow i o a (Maybe (ThreadedArrow i o a i o),i) i
switchContinue = proc (m_thread,i) -> case m_thread of
    Just (ta@(ThreadedArrow thread)) ->
        do ThreadedArrow $ lift $ substituteThread -< Just ta
           (ThreadedArrow $ SwitchedArrow.switchContinue) -< (Just thread,i)
    Nothing -> returnA -< i

switchTerminate :: (Arrow a,ArrowChoice a) => ThreadedArrow i o a (Maybe (ThreadedArrow i o a i o),o) o
switchTerminate = proc (m_thread,o) -> case m_thread of
    Just (ta@(ThreadedArrow thread)) ->
        do (ThreadedArrow $ lift $ substituteThread) -< Just ta
           (ThreadedArrow $ SwitchedArrow.switchTerminate) -< (Just thread,o)
    Nothing -> returnA -< o

substituteThread :: (Arrow a) => StateArrow (ThreadInfo a i o) a (Maybe (ThreadedArrow i o a i o)) ()
substituteThread = 
    proc thread -> do thread_info <- fetch -< ()
                      store -< thread_info { ti_active_thread = thread }
\end{code}

\subsection{Threading Operators}

\texttt{spawnThreads} adds the specified threads to the waiting queue.  They will run immediately after
the current thread.

\texttt{killThreadIf}, if the boolean parameter is set, ends the current thread immediately.

\begin{code}
spawnThreads :: (Arrow a,ArrowChoice a) => ThreadedArrow i o a [ThreadedArrow i o a i o] ()
spawnThreads = ThreadedArrow $ lift $ 
    proc threads -> do thread_info <- fetch -< ()
                       store -< thread_info { ti_waiting_threads = threads ++ ti_waiting_threads thread_info }

killThreadIf :: (Arrow a,ArrowChoice a) => ThreadedArrow i o a (Bool,o) o
killThreadIf = proc (b,o) -> if b
    then do (ThreadedArrow $ lift $ substituteThread) -< Nothing
	    (ThreadedArrow $ SwitchedArrow.switchTerminate) -< 
	        (Just $ arr (error "ThreadedArrow: killThreadIf: tried to use a killed thread"),o)
    else returnA -< o
\end{code}

\subsection{The StatefulArrow form of a ThreadedArrow}

\begin{code}
statefulForm :: (ArrowChoice a,ArrowApply a) => [ThreadedArrow i o a i o] -> StatefulArrow a i [o]
statefulForm = stateContext $
    proc i -> do threads <- fetch -< ()
                 (o,thread_result) <- lift (runState runThreads) -< (i,initialThreadedState threads)
                 store -< ti_completed_threads thread_result
		 returnA -< o
\end{code}
