module Main (main) where

import System.Process
import System.Exit
import Control.Concurrent
import System.Environment
import System.FilePath
import System.IO
import Control.Monad
import Paths_roguestar_gl

main :: IO ()
main = 
    do (should_echo_protocol,args) <- 
           do args <- getArgs
	      return ("--echo-protocol" `elem` args,
		      filter (/= "--echo-protocol") $ args)
       n <- getNumberOfCPUCores
       bin_dir <- getBinDir
       let n_rts_string = if n == 1 then [] else ["-N" ++ show n]
       let gl_args = ["+RTS", "-G4"] ++ n_rts_string ++ ["-RTS"] ++ args
       let engine_args = ["+RTS"] ++ n_rts_string ++ ["-RTS"] ++ ["version","over","begin"]
       (e_in,e_out,e_err,roguestar_engine) <- runInteractiveProcess (bin_dir `combine` "roguestar-engine") engine_args Nothing Nothing
       (gl_in,gl_out,gl_err,roguestar_gl) <- runInteractiveProcess (bin_dir `combine` "roguestar-gl") gl_args Nothing Nothing
       forkIO $ pump e_out  $ [("",gl_in)] ++ (if should_echo_protocol then [("engine >>> gl *** ",stdout)] else [])
       forkIO $ pump gl_out $ [("",e_in)] ++ (if should_echo_protocol then [("gl <<< engine *** ",stdout)] else [])
       forkIO $ pump e_err  $ [("roguestar-engine *** ",stderr)]
       forkIO $ pump gl_err $ [("roguestar-gl     *** ",stderr)]
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

pump :: Handle -> [(String,Handle)] -> IO ()
pump from tos =
    do mapM_ (flip hSetBuffering NoBuffering . snd) tos
       hSetBuffering from NoBuffering
       forever $
           do l <- hGetLine from
	      flip mapM_ tos $ \(name,to) -> 
	          do hPutStrLn to $ name ++ l
	             hFlush to

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
