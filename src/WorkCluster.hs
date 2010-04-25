module WorkCluster
    (WorkCluster,
     WorkRequestType(..),
     WorkRequest,
     WorkResult,
     newWorkCluster,
     workRequest,
     replaceWorkOperation)
    where

import PrioritySync.PrioritySync
import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Data.IORef
import qualified Data.ByteString as B
import qualified Data.PSQueue as PSQ
import qualified Data.Map as Map
import DB

-- | Measure the number of 'Query's between 'Action's, and add this number to get the maximum size of the most-recent-request queue.
queue_size_bonus :: Int
queue_size_bonus = 10

data WorkRequestType = Query | Action deriving (Eq,Ord,Show)
type WorkRequest = (WorkRequestType,[B.ByteString])
type WorkResult = Either DBError (B.ByteString,DB_BaseType)

data WorkPrio = Now | Eventually deriving (Eq,Ord)

data WorkClusterData = WorkClusterData {
    wc_queue_max_size :: IORef Int,            -- how large the most-recent-requests queue should be allowed to grow
    wc_recent_request_count :: IORef Int,      -- number of manual requests since the last 'replaceWorkOperation'
    wc_abort_counter :: IORef Integer,         -- increments on every 'replaceWorkOperation'
    wc_request_counter :: IORef Integer,       -- increments on every manual request
    wc_recent_requests :: IORef (PSQ.PSQ WorkRequest Integer),
    wc_task_pool :: IORef (TaskPool WorkPrio ()),
    wc_request_operation :: IORef (WorkRequest -> IO WorkResult),
    wc_task_handles :: IORef (Map.Map WorkRequest (TaskHandle WorkPrio WorkResult)) }

data WorkCluster = WorkCluster (MVar WorkClusterData)

newWorkCluster :: IO WorkCluster
newWorkCluster = liftM WorkCluster . newMVar =<< (WorkClusterData <$>
    newIORef 0 <*> newIORef 0 <*> newIORef 0 <*> newIORef 0 <*> newIORef PSQ.empty <*> (newIORef =<< simpleTaskPool) <*>
    newIORef (error "newWorkCluster: please call 'replaceWorkOperation' at least once") <*>
    newIORef Map.empty)

startWork :: WorkCluster -> WorkRequest -> WorkPrio -> IO (TaskHandle WorkPrio WorkResult)
startWork (WorkCluster wc_data_var) work_request prio = liftM snd $ withMVar wc_data_var $ \wc_data ->
    do when (prio == Now) $ 
           do modifyIORef (wc_recent_request_count wc_data) succ
              modifyIORef (wc_request_counter wc_data) succ
              c <- readIORef $ wc_request_counter wc_data
              modifyIORef (wc_recent_requests wc_data) $ PSQ.insertWith max work_request c
              queue_max_size <- readIORef $ wc_queue_max_size wc_data
              modifyIORef (wc_recent_requests wc_data)   $ \psq -> if PSQ.size psq >= queue_max_size then PSQ.deleteMin psq else psq
       handles <- readIORef (wc_task_handles wc_data)
       case (Map.lookup work_request handles) of
           Just handle -> 
               do reprioritize handle $ min prio
                  return (wc_data, handle)
           Nothing -> 
               do pool <- readIORef $ wc_task_pool wc_data
                  op <- readIORef $ wc_request_operation wc_data
                  result <- dispatch (schedule pool prio) $ op work_request
                  modifyIORef (wc_task_handles wc_data) $ Map.insert work_request result
                  return (wc_data, result)
       
workRequest :: WorkCluster -> WorkRequest -> IO WorkResult
workRequest work_cluster work_request = getResult =<< startWork work_cluster work_request Now

replaceWorkOperation :: WorkCluster -> (WorkRequest -> IO WorkResult) -> IO ()
replaceWorkOperation (WorkCluster wc_data_var) op = 
    do recent_requests <- liftM snd $ withMVar wc_data_var $ \wc_data ->
           do pool <- readIORef $ wc_task_pool wc_data
              startQueue pool
              modifyIORef (wc_task_handles wc_data) $ const Map.empty
              modifyIORef (wc_abort_counter wc_data) succ
              id $ do queue_max_size <- readIORef $ wc_queue_max_size wc_data -- grow/shrink the most-recently-used request queue according to actual usage
                      recent_request_count <- readIORef $ wc_recent_request_count wc_data
                      writeIORef (wc_recent_request_count wc_data) 0
                      writeIORef (wc_queue_max_size wc_data) $ max (queue_max_size - 1 :: Int) ((recent_request_count :: Int) + queue_size_bonus)
              key_abort_index <- readIORef $ wc_abort_counter wc_data
              writeIORef (wc_request_operation wc_data) $ \wo ->
                  do current_abort_index <- readIORef $ wc_abort_counter wc_data
                     case key_abort_index == current_abort_index of
                         False -> return $ error "replaceWorkOperation: operation aborted"
                         True -> op wo
              liftM ((,) wc_data . PSQ.keys) $ readIORef $ wc_recent_requests wc_data
       forM_ recent_requests $ \work_request -> startWork (WorkCluster wc_data_var) work_request Eventually

