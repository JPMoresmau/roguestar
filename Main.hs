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
       forkIO $ mapM_ putStrLn =<< liftM (map ("*** " ++) . lines) (hGetContents err)
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
    do m_nop <- readNumberOfProcessors
       m_cpuinfo <- scanCPUInfo
       case (m_nop,m_cpuinfo) of
           (Just n,_) -> do hPutStrLn stderr $ "roguestar: " ++ show n ++ 
	                        " CPU core(s) based on windows environment variable NUMBER_OF_PROCESSORS. "
			    return n
           (_,Just n) -> do hPutStrLn stderr $ "roguestar: " ++ show n ++ " CPU core(s) based on /proc/cpuinfo. " 
	                    return n
	   _ ->          do hPutStrLn stderr "roguestar: couldn't find number of CPU cores, assuming 1 core"
	                    return 1

readNumberOfProcessors :: IO (Maybe Int)
readNumberOfProcessors = flip catch (const $ return Nothing) $ liftM Just $
    do n <- getEnv "NUMBER_OF_PROCESSORS"
       return $! read n

scanCPUInfo :: IO (Maybe Int)
scanCPUInfo = catch (liftM (Just . length . filter isProcessorLine . map words . lines) $ readFile "/proc/cpuinfo")
                    (const $ return Nothing)
    where isProcessorLine ["processor",":",_] = True
          isProcessorLine _ = False
