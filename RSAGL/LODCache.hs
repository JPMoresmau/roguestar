module RSAGL.LODCache
    (LODCache,newLODCache,getLOD)
    where

import Data.Map as Map
import Control.Concurrent
import Control.Monad
import Data.Maybe
import RSAGL.Bottleneck
import Control.Exception
import Data.List

data LODCache q a = LODCache Bottleneck (q -> IO a) (MVar [q]) (MVar (Map q a))

-- | Construct a new LODCache.  This will not return until at least one level of detail has been constructed.
-- Accepts a Bottleneck object, an action that constructs an entity at a given level of detail, and a list
-- of levels-of-detail that should be generated.
newLODCache :: (Ord q) => Bottleneck -> (q -> IO a) -> [q] -> IO (LODCache q a)
newLODCache _ _ [] = error "newLODCache: empty quality list"
newLODCache bottleneck f qss =
    do let (q:qs) = sort qss
       lowest_quality <- evaluate =<< f q
       liftM2 (LODCache bottleneck f) (newMVar qs) (newMVar $ singleton q lowest_quality)

completeLOD :: (Ord q) => LODCache q a -> q -> IO ()
completeLOD (lod_cache@(LODCache bottleneck f quality_mvar map_mvar)) want_q = bracketOnError (takeMVar quality_mvar) (tryPutMVar quality_mvar) $ \qualities ->
       case qualities of
           (q:qs) | q <= want_q -> do new_elem <- constrict bottleneck $ evaluate =<< (f q)
                                      modifyMVar_ map_mvar (return . Map.insert q new_elem)
                                      putMVar quality_mvar qs
                                      completeLOD lod_cache want_q
           _ -> do putMVar quality_mvar qualities

-- | Retrieve a level-of-detail entity from this 'LODCache'.  'getLOD' will try to find the highest-quality entity
-- less than the specified quality.  'getLOD' always returns immediately; but will fire off a worker thread to
-- construct higher quality entities if it would have preferred to return one.
getLOD :: (Ord q) => LODCache q a -> q -> IO a
getLOD (lod_cache@(LODCache _ _ quality_mvar mv)) q = 
    do m <- readMVar mv
       case Map.lookup q m of
            Just a -> return a
            Nothing -> do e <- isEmptyMVar quality_mvar -- is completeLOD already running or pending for this LODCache?
                          when (not e) $ (forkIO $ completeLOD lod_cache q) >> return ()  -- then don't launch another one
                          let suitable_qualities = filterWithKey (\k _ -> (k <= q)) m
                          return $ snd $ if Map.null suitable_qualities 
                              then findMin m
                              else findMax suitable_qualities 
