{-# LANGUAGE ExistentialQuantification, FlexibleInstances, OverloadedStrings #-}

module Main (main) where

import System.Process
import System.Exit
import Control.Concurrent
import System.Environment
import System.FilePath
import System.IO
import Control.Monad
import Paths_roguestar_gl
import GHC.Environment
import System.Time
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import GHC.Exts (IsString(..))

known_args :: [String]
known_args = [arg_echo_protocol,arg_single_threaded]

arg_echo_protocol :: String
arg_echo_protocol = "--echo-protocol"

arg_single_threaded :: String
arg_single_threaded = "--single-threaded"

main :: IO ()
main = 
    do (should_echo_protocol,single_threaded,args) <- 
           do args <- getFullArgs
	      return (arg_echo_protocol `elem` args,
                      arg_single_threaded `elem` args,
		      filter (not . (`elem` known_args)) $ args)
       n <- getNumberOfCPUCores
       bin_dir <- getBinDir
       let n_rts_string = if n == 1 || single_threaded then [] else ["-N" ++ show n]
       let gl_args = ["+RTS", "-G4"] ++ n_rts_string ++ ["-RTS"] ++ args
       let engine_args = ["+RTS"] ++ n_rts_string ++ ["-RTS"] ++ args ++ ["version","over","begin"]
       let roguestar_engine_bin = bin_dir `combine` "roguestar-engine"
       let roguestar_gl_bin = bin_dir `combine` "roguestar-gl"
       when ("--verbose" `elem` args) $ putStrLn $ "starting process: " ++ roguestar_engine_bin ++ " " ++ unwords engine_args
       (e_in,e_out,e_err,roguestar_engine) <- runInteractiveProcess (bin_dir `combine` "roguestar-engine") engine_args Nothing Nothing
       when ("--verbose" `elem` args) $ putStrLn $ "starting process: " ++ roguestar_gl_bin ++ " " ++ unwords gl_args
       (gl_in,gl_out,gl_err,roguestar_gl) <- runInteractiveProcess (bin_dir `combine` "roguestar-gl") gl_args Nothing Nothing
       stdout_chan <- newChan
       _ <- forkIO $ pump e_out  $ [("",DHandle gl_in)] ++ (if should_echo_protocol then [("engine >>> gl *** ",DChanTime stdout_chan)] else [])
       _ <- forkIO $ pump gl_out $ [("",DHandle e_in)] ++ (if should_echo_protocol then [("engine <<< gl *** ",DChanTime stdout_chan)] else [])
       _ <- forkIO $ pump e_err  $ [("roguestar-engine *** ",DChan stdout_chan)]
       _ <- forkIO $ pump gl_err $ [("roguestar-gl     *** ",DChan stdout_chan)]
       _ <- forkIO $ printChan stdout stdout_chan
       _ <- forkIO $
           do roguestar_engine_exit <- waitForProcess roguestar_engine
              case roguestar_engine_exit of
                  ExitFailure x -> putStrLn $ "roguestar-engine terminated unexpectedly (" ++ show x ++  ")"
                  _ -> return ()
	      return ()
       roguestar_gl_exit <- waitForProcess roguestar_gl
       case roguestar_gl_exit of
           ExitFailure x -> putStrLn $ "roguestar-gl terminated unexpectedly (" ++ show x ++ ")"
           _ -> return ()       

data Destination = DHandle Handle | DChan (Chan B.ByteString) | DChanTime (Chan B.ByteString)

send :: Destination -> B.ByteString -> IO ()
send (DHandle h) str = B.hPutStrLn h str >> hFlush h
send (DChan c) str = writeChan c str
send (DChanTime c) str = 
    do t <- getClockTime
       writeChan c $ fromString (show t) `B.append` ": " `B.append` str

setup :: Destination -> IO ()
setup (DHandle h) = hSetBuffering h NoBuffering
setup _ = return ()

pump :: Handle -> [(B.ByteString,Destination)] -> IO ()
pump from tos =
    do mapM_ (setup . snd) tos
       forever $
           do l <- B.hGetLine from
	      flip mapM_ tos $ \(name,to) -> send to $ name `B.append` l

printChan :: Handle -> Chan B.ByteString -> IO ()
printChan h c = forever $
    do s <- readChan c
       B.hPutStrLn h s
       hFlush h

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
