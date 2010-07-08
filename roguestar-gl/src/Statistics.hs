module Statistics
    (Statistics,newStatistics,runStatistics)
    where

import qualified Data.Vector.Unboxed as V
import Statistics.Sample as S
import Data.List as L
import Statistics.Quantile
import Control.Concurrent
import RSAGL.FRP.Time
import System.IO
import System.IO.Unsafe
import RSAGL.Math.AbstractVector
import Control.Monad
import RSAGL.Math.Types
import Text.Printf
import Control.Exception

{-# NOINLINE error_pump #-}
error_pump :: Chan String
error_pump = unsafePerformIO $
    do pump <- newChan
       _ <- forkIO $ forever $
                do hPutStrLn stderr =<< readChan pump
       return pump

data Statistics = Statistics {
    statistics_sample :: MVar [Double],
    statistics_max :: MVar Double,
    statistics_avg :: MVar Double,
    statistics_time :: MVar Time,  -- time since last flushing of statistics
    statistics_name :: String }

newStatistics :: String -> IO Statistics
newStatistics name =
    do sample <- newMVar []
       t <- newMVar =<< getTime
       s_max <- newMVar 0
       s_avg <- newMVar 1000
       return $ Statistics {
           statistics_sample = sample,
           statistics_max = s_max,
           statistics_avg = s_avg,
           statistics_time = t,
           statistics_name = name }

runStatistics :: Statistics -> IO a -> IO a
runStatistics stats action =
    do t_start <- getTime
       a <- action
       t_end <- getTime
       let t_diff = f2f $ toSeconds $ t_end `sub` t_start
       modifyMVar_ (statistics_sample stats) $ return . (t_diff :)
       modifyMVar_ (statistics_max stats) $ return . (max t_diff)
       modifyMVar_ (statistics_avg stats) $ return . (\t -> (99*t + t_diff)/100)
       _ <- evaluate =<< readMVar (statistics_max stats)
       _ <- evaluate =<< readMVar (statistics_avg stats)
       modifyMVar_ (statistics_time stats) $ \t ->
           do let should_report = (toSeconds (t_end `sub` t) > 30)
              when should_report $ liftM (const ()) $ forkIO $
                  do modifyMVar_ (statistics_sample stats) $ \x ->
                         do let sv = V.fromList $ take (60*30) x :: Sample
                            let f = printf "%.4f" . (*60) :: Double -> String
                            writeChan error_pump $ "\nSTATISTICS " ++ statistics_name stats ++
                                                   "\nMEAN       " ++ f (mean sv) ++
                                                   "\nSTD DEV    " ++ f (stdDev sv) ++
                                                   "\nKURTOSIS   " ++ f (kurtosis sv) ++
                                                   "\nQUARTILES  " ++ (concat $ intersperse " | " $ map f $
				[continuousBy medianUnbiased 0 4 sv,
                                 continuousBy medianUnbiased 1 4 sv,
                                 continuousBy medianUnbiased 2 4 sv,
                                 continuousBy medianUnbiased 3 4 sv,
                                 continuousBy medianUnbiased 4 4 sv])
                            writeChan error_pump =<< liftM ((statistics_name stats ++ " lifetime maximum ") ++) (liftM f $ readMVar $ statistics_max stats)
                            writeChan error_pump =<< liftM ((statistics_name stats ++ " floating average ") ++) (liftM f $ readMVar $ statistics_avg stats)
                            return []
              return $ if should_report then t_end else t
       return a

