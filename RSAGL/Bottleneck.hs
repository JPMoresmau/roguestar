{-# LANGUAGE ScopedTypeVariables #-}
module RSAGL.Bottleneck
    (constrict,dispatch,simpleBottleneck,Bottleneck)
    where

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import System.IO

-- | A simple thread pool bottleneck, used to restrict work to one or more worker threads.
newtype Bottleneck = Bottleneck {
    bottleneck_chan :: Chan (IO ()) }

-- | Single thread bottleneck.
simpleBottleneck :: IO (Bottleneck)
simpleBottleneck = 
    do bottleneck <- liftM Bottleneck newChan
       forkIO $ work bottleneck
       return bottleneck

work :: Bottleneck -> IO ()
work bottleneck = forever $
    do actionM <- readChan $ bottleneck_chan bottleneck
       catch actionM $ \(e :: SomeException) ->
           do hPutStr stderr "RSAGL.Bottleneck: exception thrown from bottleneck worker thread."
              hPutStr stderr $ show e

-- | Dispatch a job to the thread pool and block until the job completes.
constrict :: Bottleneck -> IO a -> IO a
constrict bottleneck actionM = takeMVar =<< dispatch bottleneck actionM

-- | Dispatch a job to the thread pool asynchronously.
dispatch :: Bottleneck -> IO a -> IO (MVar a)
dispatch bottleneck actionM = 
    do return_mvar <- newEmptyMVar
       writeChan (bottleneck_chan bottleneck) $ putMVar return_mvar =<< actionM
       return return_mvar
