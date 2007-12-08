\section{Threaded Quality Control}

\begin{code}
module RSAGL.QualityControl
    (QualityCache,mkQuality,getQuality)
    where

import Data.DeepSeq
import Data.Map as Map
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.List as List
\end{code}

The \texttt{QualityCache} object is used to memoize entities with variable level-of-detail.

\texttt{getQuality} answers the highest-quality available object that has a quality less than or equal to the requested quality.

If necessary, the \texttt{QualityCache} fires off a worker thread to generate higher-quality versions of the entity.  At most
one worker thread is ever actually running for each \texttt{QualityCache}.

The effect when \texttt{QualityCache} is used to view 3D models is a little like loading a progressive JPEG.  First a very low
quality model appears, which is gradually replaced by higher and higher qualities until the desired level of detail is finished.
\begin{code}
data QualityCache q a = QualityCache (q -> a) (MVar [q]) (MVar (Map q a))

mkQuality :: (Ord q,DeepSeq a) => (q -> a) -> [q] -> IO (QualityCache q a)
mkQuality f qs = 
    do qo <- liftM2 (QualityCache f) (newMVar qs) (newMVar $ singleton (head qs) $ f $ head qs)
       forkIO $ completeQuality qo (head qs)
       return qo

completeQuality :: (Ord q,DeepSeq a) => QualityCache q a -> q -> IO ()
completeQuality (qo@(QualityCache f quality_mvar map_mvar)) want_q =
    do qualities <- takeMVar quality_mvar  -- block on the quality_mvar
       case qualities of
           (q:qs) | q < want_q -> do new_elem <- return $!! f q
                                     modifyMVar_ map_mvar (return . Map.insert q new_elem)
                                     threadDelay 250000 -- delay until the next call to completeQuality to back off from 100% cpu usage
                                     putMVar quality_mvar qs
                                     completeQuality qo want_q
           _ -> putMVar quality_mvar qualities

getQuality :: (Ord q,DeepSeq a) => QualityCache q a -> q -> IO a
getQuality (qo@(QualityCache _ quality_mvar mv)) q = 
    do m <- readMVar mv
       case Map.lookup q m of
            Just a -> return a
            Nothing -> do e <- isEmptyMVar quality_mvar -- is completeQuality already running for this QualityObject?
                          when (not e) $ (forkIO $ completeQuality qo q) >> return ()  -- then don't launch another one
                          return $ snd $ findMax $ filterWithKey (\k _ -> (k <= q)) m
\end{code}
