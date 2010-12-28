module Statistics
    (Statistics,newStatistics,runStatistics,
     animation_query,
     animation_post_exec)
    where

import Data.List as L
import Control.Concurrent
import RSAGL.FRP.Time
import System.IO
import System.IO.Unsafe
import RSAGL.Math.AbstractVector
import Control.Monad
import RSAGL.Math.Types
import Text.Printf
import Control.Exception
import Data.Maybe

{-# NOINLINE animation_query #-}
animation_query :: Statistics
animation_query = unsafePerformIO $ newStatistics "animation-query"

{-# NOINLINE animation_post_exec #-}
animation_post_exec :: Statistics
animation_post_exec = unsafePerformIO $ newStatistics "animation-post-exec"

{-# NOINLINE error_pump #-}
error_pump :: Chan String
error_pump = unsafePerformIO $
    do pump <- newChan
       _ <- forkIO $ forever $
                do hPutStrLn stderr =<< readChan pump
       return pump

data StatisticsBlock = StatisticsBlock {
    statistics_max :: !RSdouble,
    statistics_min :: !RSdouble,
    -- The running average.
    statistics_total :: !RSdouble,
    statistics_count :: !Integer }

empty_stats :: StatisticsBlock
empty_stats = StatisticsBlock {
    statistics_max = 0.0,
    statistics_min = 0.0,
    statistics_total = 0.0,
    statistics_count = 0 }

contributeSample :: RSdouble -> Maybe StatisticsBlock -> Maybe StatisticsBlock
contributeSample v Nothing = Just $ StatisticsBlock {
    statistics_max = v,
    statistics_min = v,
    statistics_total = v,
    statistics_count = 1 }
contributeSample v (Just stats) = Just $ stats {
    statistics_max = statistics_max stats `max` v,
    statistics_min = statistics_min stats `min` v,
    statistics_total = v + statistics_total stats,
    statistics_count = succ $ statistics_count stats }

data Statistics = Statistics {
    -- time since last flushing of statistics
    statistics_block :: MVar (Time,Maybe StatisticsBlock),
    statistics_name :: String }

newStatistics :: String -> IO Statistics
newStatistics name =
    do t <- getTime
       new_block <- newMVar (t,Nothing)
       return $ Statistics {
           statistics_block = new_block,
           statistics_name = name }

runStatistics :: Statistics -> IO a -> IO a
runStatistics stats_object action =
    do t_start <- getTime
       a <- action
       t_end <- getTime
       let t_diff = f2f $ toSeconds $ t_end `sub` t_start
       modifyMVar_ (statistics_block stats_object) $ \(t,m_stats) ->
           do let should_report = toSeconds (t_end `sub` t) > 60 && isJust m_stats
                  stats = fromMaybe empty_stats m_stats
              when should_report $
                  do let f = printf "%.4f" . (f2f :: RSdouble -> Double) :: RSdouble -> String
                     writeChan error_pump $ "\nSTATISTICS            " ++ statistics_name stats_object ++
                                            "\nMEAN                  " ++ f (statistics_total stats / fromInteger (statistics_count stats)) ++
                                            "\nMAX                   " ++ f (statistics_max stats) ++
                                            "\nMIN                   " ++ f (statistics_min stats) ++
                                            "\nN                     " ++ show (statistics_count stats) ++
                                            "\n% TIME                " ++ f (statistics_total stats / toSeconds (t_end `sub` t) * 100.0) ++
                                            "\nTHROUGHPUT            " ++ f (fromInteger (statistics_count stats) / statistics_total stats) ++
                                            "\nEVENTS PER SECOND     " ++ f (fromInteger (statistics_count stats) / toSeconds (t_end `sub` t))
              seq stats $ return $ if should_report then (t_end,Nothing) else (t,contributeSample t_diff m_stats)
       return a

