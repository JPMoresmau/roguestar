\section{Artificial Bottlenecks for Parallel Computation}

In the absence of a thread priority support for haskell, and the desire not to overwhelm the rendering thread with many non-essential modeling threads, we implement an artificial bottleneck that restricts at a very coarse level the number of parallel computations that may take place at any one time.

By claiming a bottleneck, we ensure that no other operation can run on that bottleneck until the bottleneck is released.  This is essentially a semaphore, but the goal is related to performance rather than correctness.

\begin{code}
module RSAGL.Bottleneck
    (constrict,newBottleneck,Bottleneck)
    where

import Control.Monad
import Control.Concurrent.MVar
\end{code}

\begin{code}
newtype Bottleneck = Bottleneck (MVar ())

newBottleneck :: IO (Bottleneck)
newBottleneck = liftM Bottleneck $ newMVar ()

constrict :: Bottleneck -> IO a -> IO a
constrict bottleneck action =
    do claim bottleneck
       result <- action
       release bottleneck
       return result

claim :: Bottleneck -> IO ()
claim (Bottleneck mv) = takeMVar mv

release :: Bottleneck -> IO ()
release (Bottleneck mv) = putMVar mv ()
\end{code}