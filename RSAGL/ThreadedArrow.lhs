\section{RSAGL.ThreadedArrow}

\label{MustBeAMonoid}

A ThreadedArrow is an extension of the SwitchedArrow.  In addition to switching, a ThreadedArrow can spawn new
switches that run independantly.  All switches recieve the same input.  The output type must be a monoid.
\footnote{See http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Monoid.html}  The output is then
combined using Monoid's mappend function.  Finally, threads can terminate.  If all threads terminate,
the result of the ThreadedArrow becomes mempty.

While the ThreadedArrow has conceptual similarities to the threading systems provided by an operating system,
it does not provide true parallelism or concurency.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.ThreadedArrow
    (ThreadedFunction,
     ThreadedSwitch,
     ThreadedArrow,
     ArrowThreaded(..),
     killThread,
     threadedContext,
     spawnThreadsDelayed)
    where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import RSAGL.SwitchedArrow
import RSAGL.StatefulArrow
import Data.Monoid
import Data.Maybe
\end{code}

The ThreadedSwitch is the threaded form of the Switch, as seen in the SwitchedArrow. \footnote{page \pageref{Switch}}

\begin{code}
type ThreadedSwitch a i o = ThreadedArrow i o a i o

type ThreadedFunction i o = ThreadedArrow i o (->)
\end{code}

The ThreadedArrow itself is a form of SwitchedArrow.  It retains state (ThreadInfo) that
indicates what threads have completed, are waiting to be completed, and are currently executing.

As threads complete, they are added to the completed queue, and the next waiting thread is made active.

When all of the threads have completed, the queue of waiting threads is set to the list
of completed threads, and the cycle begins again.

\begin{code}
data ThreadInfo a i o = ThreadInfo { ti_active_thread :: Maybe (ThreadedSwitch a i o),
                                     ti_waiting_threads :: [ThreadedSwitch a i o],
                                     ti_completed_threads :: [ThreadedSwitch a i o] }

newtype ThreadedArrow i o a j p = ThreadedArrow (SwitchedArrow i o (StateArrow (ThreadInfo a i o) a) j p)

initialThreadedState :: [ThreadedSwitch a b c] -> ThreadInfo a b c
initialThreadedState threads = ThreadInfo { ti_active_thread = Nothing,
                                            ti_waiting_threads = threads,
                                            ti_completed_threads = [] }

loadNextThread :: (Arrow a) => StateArrow (ThreadInfo a b c) a () (ThreadInfo a b c)
loadNextThread = fetch >>> arr loadNextThread_ >>> store >>> fetch
    where loadNextThread_ thread_info =
              case thread_info of
                  t@(ThreadInfo { ti_active_thread = Nothing, ti_waiting_threads = (thread:threads) }) -> 
                           (t { ti_active_thread = Just thread,
                                ti_waiting_threads = threads })
                  t -> t

storeCurrentThread :: (Arrow a) => StateArrow (ThreadInfo a b c) a () ()
storeCurrentThread = fetch >>> arr storeCurrentThread_ >>> store
    where storeCurrentThread_ thread_info =
              thread_info { ti_completed_threads = maybeToList (ti_active_thread thread_info) ++ 
                                                   ti_completed_threads thread_info,
                            ti_active_thread = Nothing }

runThread :: (Arrow a,ArrowChoice a) => ThreadedSwitch a i o -> StateArrow (ThreadInfo a i o) a i o
runThread (ThreadedArrow thread) = 
    let (StatefulArrow switched_form) = switchedContext thread
        in proc i -> do (o,_) <- switched_form -< i
                        storeCurrentThread -< ()
                        returnA -< o

runThreads :: (Monoid o,Arrow a,ArrowApply a,ArrowChoice a) => StateArrow (ThreadInfo a i o) a i o
runThreads = proc i ->
    do thread_info <- loadNextThread -< ()
       case ti_active_thread thread_info of
           Just thread -> do o <- app -< (runThread thread,i)
                             os <- runThreads -< i
                             returnA -< o `mappend` os
           Nothing -> returnA -< mempty

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

instance ArrowSwitched ThreadedArrow where
    switchContinue = proc (ta@(ThreadedArrow cont),i) ->
        do ThreadedArrow $ lift $ substituteThreadPrim -< ta
           (ThreadedArrow $ switchContinue) -< (cont,i)
    switchTerminate = proc (ta@(ThreadedArrow cont),o) ->
        do (ThreadedArrow $ lift $ substituteThreadPrim) -< ta
           (ThreadedArrow $ switchTerminate) -< (cont,o)
\end{code}

\subsection{The ArrowThreaded typeclass}

There are two thread-related functions: spawnThreads and killThread.

spawnThreads adds the specified threads to the waiting queue.  They will run immediately after
the current thread.  The current thread continues indefinitely.

killThread ends the current thread immediately, returning the specified result.

To provide flow control where it might not otherwise exist, killThreadIf executes
only if the boolean parameter is True.  

\begin{code}
class ArrowThreaded a where
    spawnThreads :: (Arrow b,ArrowChoice b) => a i o b [a i o b i o] ()
    killThreadIf :: (Arrow b,ArrowChoice b,Monoid o) => a i o b (Bool,o) o

instance ArrowThreaded ThreadedArrow where
    spawnThreads = spawnThreadsPrim
    killThreadIf = killThreadIfPrim

killThread :: (Arrow (a i o b),ArrowThreaded a,Arrow b,ArrowChoice b,Monoid o) => a i o b o o
killThread = proc o -> do killThreadIf -< (True,o)

substituteThreadPrim :: (Arrow a) => StateArrow (ThreadInfo a i o) a (ThreadedSwitch a i o) ()
substituteThreadPrim = 
    proc thread -> do thread_info <- fetch -< ()
                      store -< thread_info { ti_active_thread = Just thread }

spawnThreadsPrim :: (Arrow a,ArrowChoice a) => ThreadedArrow i o a [ThreadedSwitch a i o] ()
spawnThreadsPrim = ThreadedArrow $ lift $ 
        proc threads -> do thread_info <- fetch -< ()
                           store -< thread_info { ti_waiting_threads = threads ++ ti_waiting_threads thread_info }

killThreadIfPrim :: (Arrow a,ArrowChoice a,Monoid o) => ThreadedArrow i o a (Bool,o) o
killThreadIfPrim = proc (b,o) ->
    do if b
           then switchTerminate -< (ThreadedArrow $ lift $ 
                                    fetch >>> arr killThreadPrim_ >>> store >>> arr (const mempty), o)
           else returnA -< o
    where killThreadPrim_ thread_info = thread_info { ti_active_thread = Nothing }
\end{code}

\subsection{Avoiding non-termination in spawnThreads}

spawnThreadsDelayed spawns the specified threads, but runs them one iteration after this thread finishes.
This can mitigate certain out-of-control loops when threads continuously spawn new threads.

\begin{code}
spawnThreadsDelayed :: (Arrow (at i o a),ArrowThreaded at,ArrowSwitched at, Arrow a,ArrowChoice a,Monoid o) => 
                       at i o a [at i o a i o] ()
spawnThreadsDelayed = spawnThreads <<< (arr $ map delayThread_)
    where delayThread_ thread = proc _ -> do switchTerminate -< (thread,mempty)
\end{code}

\subsection{Embedding a ThreadedArrow as a StatefulArrow}

Like the SwitchedArrow \footnote{page \pageref{switchedContext}} a ThreadedArrow can be made to appear as a StatefulArrow.

\begin{code}
threadedContext :: (Monoid o,ArrowChoice a,ArrowApply a) => [ThreadedSwitch a b o] -> StatefulArrow a b o
threadedContext = stateContext $
    proc i -> do threads <- fetch -< ()
                 (o,thread_result) <- lift (runState runThreads) -< (i,initialThreadedState threads)
                 store -< ti_completed_threads thread_result
		 returnA -< o
\end{code}