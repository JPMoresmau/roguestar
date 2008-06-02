module Main (main) where
{- This is a small shell wrapper.
   What we do is we use the 'netpipes' program to set up a socket, and then we
   tell the shell to set the roguestar server running; we pause briefly to give
   it a chance to set up the socket. Then we open up the nice GL
   interface.
   The 'threadDelay' bit is ugly; we need it because if either one opens up and
   can't find the socket at 5618, it'll just exit - it won't wait for the other
   one to start up. It's a race condition, alas. -}
import System.Cmd (system)
import System.Exit
import Control.Concurrent

main :: IO ()
main =  do forkIO $ 
               do putStrLn "Launching engine..."
                  ec <- system $ "faucet 5618 --out --in --unix --once " ++ "roguestar-engine version over begin"
                  case ec of
                      ExitFailure x -> putStrLn $ "roguestar-engine terminated unexpectedly (" ++ show x ++ ")"
                      _ -> return ()
                  return ()
           threadDelay 2000000
           putStrLn "Launching OpenGL client..."
           ec <- system $ "hose localhost 5618 --out --in --unix " ++ "roguestar-gl"
           case ec of
               ExitFailure x -> putStrLn $ "roguestar-gl terminated unexpectedly (" ++ show x ++ ")"
               _ -> return()
           return ()