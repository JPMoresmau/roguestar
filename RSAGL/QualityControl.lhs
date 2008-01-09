\section{Threaded Quality Control}

\begin{code}
module RSAGL.QualityControl
    (QualityCache,newQuality,getQuality)
    where

import Control.Parallel.Strategies
import Data.Map as Map
import Control.Concurrent
import Control.Monad
import Data.Maybe
import RSAGL.Bottleneck
\end{code}

The \texttt{QualityCache} object is used to memoize entities with variable level-of-detail.

\texttt{QualityCache}s use \texttt{Bottlenecks} to limit the amount of non-essential computation that is taking place at any one time.

\texttt{getQuality} answers the highest-quality available object that has a quality less than or equal to the requested quality.

If necessary, the \texttt{QualityCache} fires off a worker thread to generate higher-quality versions of the entity.  At most
one worker thread is ever actually running for each \texttt{QualityCache}.

The effect when \texttt{QualityCache} is used to view 3D models is a little like loading a progressive JPEG.  First a very low
quality model appears, which is gradually replaced by higher and higher qualities until the desired level of detail is finished.
\begin{code}
data QualityCache q a = QualityCache Bottleneck (Strategy a) (q -> a) (MVar [q]) (MVar (Map q a))

newQuality :: (Ord q,NFData a) => Bottleneck -> Strategy a -> (q -> a) -> [q] -> IO (QualityCache q a)
newQuality _ _ _ [] = error "mkQuality: empty quality list"
newQuality bottleneck strategy f (q:qs) = lowest_quality `seq` liftM2 (QualityCache bottleneck strategy f) (newMVar qs) (newMVar $ singleton q lowest_quality)
    where lowest_quality = f q `using` strategy

completeQuality :: (Ord q,NFData a) => QualityCache q a -> q -> IO ()
completeQuality (qo@(QualityCache bottleneck strategy f quality_mvar map_mvar)) want_q =
    do qualities <- takeMVar quality_mvar  -- block on the quality_mvar
       case qualities of
           (q:qs) | q < want_q -> do new_elem <- constrict bottleneck $ return $| strategy $ f q
                                     modifyMVar_ map_mvar (return . Map.insert q new_elem)
                                     putMVar quality_mvar qs
                                     completeQuality qo want_q
           _ -> do putMVar quality_mvar qualities

getQuality :: (Ord q,NFData a) => QualityCache q a -> q -> IO a
getQuality (qo@(QualityCache _ _ _ quality_mvar mv)) q = 
    do m <- readMVar mv
       case Map.lookup q m of
            Just a -> return a
            Nothing -> do e <- isEmptyMVar quality_mvar -- is completeQuality already running or pending for this QualityObject?
                          when (not e) $ (forkIO $ completeQuality qo q) >> return ()  -- then don't launch another one
                          return $ snd $ findMax $ filterWithKey (\k _ -> (k <= q)) m
\end{code}
