{-# LANGUAGE ExistentialQuantification, FlexibleInstances, OverloadedStrings #-}

module Main (main) where

import System.Process
import System.Exit
import System.Console.GetOpt
import Control.Concurrent
import System.FilePath
import System.IO
import Control.Monad
import Paths_roguestar_gl
import System.Environment
import System.Time
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import GHC.Exts (IsString(..))

data Args = Args {
    arg_echo_protocol :: Bool,
    arg_single_threaded :: Bool,
    arg_verbose :: Bool,
    arg_help :: Bool,
    arg_prefix :: String,
    arg_engine :: [String],
    arg_client :: [String] }

roguestar_options :: [OptDescr (Args -> Args)]
roguestar_options =
    [Option "s1" ["single-threaded"]
            (NoArg $ \a -> a { arg_single_threaded = True })
            "Run the engine and client single-threaded.",
     Option "e" ["echo-protocol"]
            (NoArg $ \a -> a { arg_echo_protocol = True })
            "Echo the client-server protocol (for debugging).",
     Option "h?" ["help"]
            (NoArg $ \a -> a { arg_help = True })
            "Print this help message.",
     Option "v" ["verbose"]
            (NoArg $ \a -> a { arg_verbose = True })
            "Print extra information.",
     Option "" ["debug-engine"]
            (NoArg $ \a -> a { arg_verbose = True,
                               arg_engine = "debug" : arg_engine a })
            "Spew debugging information from the engine. (implies --verbose).",
     Option "p" ["prefix","path"]
            (ReqArg (\s a -> a { arg_prefix = s }) "PREFIX")
            ("Path to the directory where the roguestar-engine and " ++
             "roguestar-gl binaries are kept.  You might need this " ++
             "to play roguestar off a remotely mounted file server " ++
             "or if something is broken and you know what you're doing.")]

help_text :: String
help_text = usageInfo
            ("Roguestar: A science-fiction role playing game.\n\n" ++
             "Roguestar has a client-server architecture (roguestar-gl, " ++
             "and roguestar-engine).  This program instantiates both " ++
             "on the local host and manages all communication between " ++
             "them.\n\n" ++
             "Usage: roguestar [OPTIONS]")
            roguestar_options

main :: IO ()
main =
    do bin_dir <- getBinDir
       let default_args = Args {
               arg_echo_protocol = False,
               arg_single_threaded = False,
               arg_help = False,
               arg_prefix = bin_dir,
               arg_verbose = False,
               arg_engine = [],
               arg_client = [] }
       raw_args <- getArgs
       let (opts_list,_,errs) = getOpt (ReturnInOrder $ \s args -> args {
                   arg_engine = arg_engine args ++ [s],
                   arg_client = arg_client args ++ [s] })
              roguestar_options
              raw_args
       let args = foldr ($) default_args opts_list
       when (arg_help args) $
           do putStrLn help_text
              exitWith ExitSuccess
       when (not $ null errs) $
           do mapM_ putStrLn errs
              exitWith ExitSuccess
       let n_rts_string = if arg_single_threaded args then [] else ["-N", "-A100m"]
       let gl_args =
               ["+RTS"] ++
               n_rts_string ++
               ["-RTS"] ++
               arg_client args
       let engine_args =
               ["+RTS"] ++
               n_rts_string ++
               ["-RTS"] ++
               arg_engine args ++
               ["version","over","begin"]
       let roguestar_engine_bin = arg_prefix args `combine` "roguestar-engine"
       let roguestar_gl_bin = arg_prefix args `combine` "roguestar-gl"
       when (arg_verbose args) $
           putStrLn $ "starting process: " ++
                      roguestar_engine_bin ++ " " ++
                      unwords engine_args
       (e_in,e_out,e_err,roguestar_engine) <-
           runInteractiveProcess roguestar_engine_bin
                                 engine_args
                                 Nothing
                                 Nothing
       when (arg_verbose args) $
           putStrLn $ "starting process: " ++
                      roguestar_gl_bin ++
                      " " ++
                      unwords gl_args
       (gl_in,gl_out,gl_err,roguestar_gl) <-
           runInteractiveProcess roguestar_gl_bin gl_args Nothing Nothing
       stdout_chan <- newChan
       _ <- forkIO $ pump e_out  $
                [("",DHandle gl_in)] ++
                (if arg_echo_protocol args
                    then [("engine >>> gl *** ",DChanTime stdout_chan)]
                    else [])
       _ <- forkIO $ pump gl_out $
                [("",DHandle e_in)] ++
                (if arg_echo_protocol args
                    then [("engine <<< gl *** ",DChanTime stdout_chan)]
                    else [])
       _ <- forkIO $ pump e_err  $ if arg_verbose args
                then [("roguestar-engine *** ",DChan stdout_chan)]
                else []
       _ <- forkIO $ pump gl_err $ if arg_verbose args
                then [("roguestar-gl     *** ",DChan stdout_chan)]
                else []
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

