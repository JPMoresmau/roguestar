module Main (main) where

import System.Process
import System.Exit
import Control.Concurrent
import System.Environment

main :: IO ()
main = 
    do args <- getArgs
       (input,out,err,roguestar_engine) <- runInteractiveProcess "roguestar-engine" ["version", "over", "begin"] 
                                           Nothing Nothing
       roguestar_gl <- runProcess "roguestar-gl" args Nothing Nothing (Just out) (Just input) Nothing
       roguestar_gl_exit <- waitForProcess roguestar_gl
       case roguestar_gl_exit of
           ExitFailure x -> putStrLn $ "roguestar-gl terminated unexpectedly (" ++ show x ++  ")"
           _ -> return ()
       roguestar_engine_exit <- waitForProcess roguestar_engine
       case roguestar_engine_exit of
           ExitFailure x -> putStrLn $ "roguestar-engine terminated unexpectedly (" ++ show x ++ ")"
           _ -> return ()

