module Main (main) where

import System.Process
import System.Exit
import Control.Concurrent
import System.Environment
import System.IO
import Control.Monad

main :: IO ()
main = 
    do args <- getArgs
       n <- getNumberOfCPUCores
       let n_rts_string = if n == 1 then [] else ["-N" ++ show n]
       let gl_args = ["+RTS", "-G4"] ++ n_rts_string ++ ["-RTS"] ++ args
       let engine_args = ["+RTS"] ++ n_rts_string ++ ["-RTS"] ++ ["version","over","begin"]
       (input,out,err,roguestar_engine) <- runInteractiveProcess "roguestar-engine" engine_args Nothing Nothing
       roguestar_gl <- runProcess "roguestar-gl" gl_args Nothing Nothing (Just out) (Just input) Nothing
       forkIO $
           do roguestar_engine_exit <- waitForProcess roguestar_engine
              case roguestar_engine_exit of
                  ExitFailure x -> putStrLn $ "roguestar-engine terminated unexpectedly (" ++ show x ++  ")"
                  _ -> return ()
	      return ()
       roguestar_gl_exit <- waitForProcess roguestar_gl
       case roguestar_gl_exit of
           ExitFailure x -> putStrLn $ "roguestar-gl terminated unexpectedly (" ++ show x ++ ")"
           _ -> return ()

getNumberOfCPUCores :: IO Int
getNumberOfCPUCores =
    do m_cpuinfo <- scanCPUInfo
       maybe (return ()) (\n -> hPutStr stderr $ "roguestar: " ++ show n ++ " CPU cores based on cpuinfo.") m_cpuinfo
       case m_cpuinfo of
           Just n -> return n
	   Nothing -> do hPutStrLn stderr "roguestar: couldn't find number of CPU cores, assuming 1 core"
	                 return 1

scanCPUInfo :: IO (Maybe Int)
scanCPUInfo = catch (liftM (Just . length . filter isProcessorLine . map words . lines) $ readFile "/proc/cpuinfo")
                    (const $ return Nothing)
    where isProcessorLine ["processor",":",_] = True
          isProcessorLine _ = False
