-- | A memoization scheme in which a piece of information is combined with it's
-- source, creating a 'Message'.  A source is the exact point from which a
-- 'Message' was instanced, and also an exact list of all modifications
-- that have been made to the 'Message'.
--
-- If we later reconstruct a new 'Message' from the same source, then we know
-- that the two 'Message's have matching contexts.
--
module RSAGL.FRP.Message
    (Message,
     consistent,
     consistency,
     peek,
     Transmitter,
     newTransmitter,
     Receiver,
     newReceiver,
     (<<*>>),
     send,
     receive,
     transmit,
     consistent,
     consistency)
    where

import System.IO.Unsafe
import Control.Concurrent.STM

-- | A sourced data stream.
data Message a = Message {
    message_source :: Source,
    message_value :: a }

-- | Two messages are consistent if they arrive from identical sources.
consistent :: Message a -> Message b -> Bool
consistent a b = message_source a == message_source b

-- | An arbitrary ordering scheme on messages.
consistency :: Message a -> Message b -> Ordering
consistency a b = message_source a `compare` message_source b

-- | Examine a message without memoization.
peek :: Message a -> a
peek = message_value

{-# NOINLINE integer_source #-}
integer_source :: TVar Integer
integer_source = unsafePerformIO $ newTVarIO 0

uniqueInteger :: STM Integer
uniqueInteger =
    do i <- readTVar integer_source
       let i' = succ i
       writeTVar integer_source i'
       return i'

data Source =
    Source !Integer
  | Apply !Int Source Source
    deriving (Eq,Ord)

data Builtin = Nest deriving (Eq,Ord)

hashSource :: Source -> Int
hashSource (Source u) = fromInteger $ u + 1000 `mod` toInteger (maxBound :: Int)
hashSource (Apply i _ _) = i

send_ :: a -> STM (Message a)
send_ a =
    do u <- uniqueInteger
       return $ Message {
           message_source = Source u,
           message_value = a }

-- | Construct a new message from a one-time source.
send :: a -> IO (Message a)
send = atomically . send_

-- | Bind two messages.
(<<*>>) :: Message (a -> b) -> Message a -> Message b
f <<*>> k = Message {
    message_source = Apply (hashSource (message_source f) + hashSource (message_source k)) (message_source f) (message_source k),
    message_value = message_value f $ message_value k }

-- | An object that can memoize sequentially matching incomming messages.
data Receiver a = Receiver {
    receiver_previous_message :: TVar (Maybe (Message a)) }

newReceiver :: IO (Receiver a)
newReceiver =
    do m <- newTVarIO Nothing
       return $ Receiver m

receive :: Receiver a -> Message a -> IO a
receive r m = atomically $
    do m_c <- readTVar $ receiver_previous_message r
       case m_c of
          Just c | message_source c == message_source m -> return $ message_value c
          _ -> 
              do writeTVar (receiver_previous_message r) $ Just m
                 return $ message_value m

data Transmitter a = Transmitter {
    transmitter_predicate :: a -> a -> Bool,
    transmitter_previous_message :: TVar (Maybe (Message a)) }

-- | An object that can memoize matching sequential outgoing messages.
-- Matching does not necessarily mean equal, for example, two outgoing floating point values might
-- be considered matching if they have the same sign and the difference between them is less than 0.01.
newTransmitterBy :: (a -> a -> Bool) -> IO (Transmitter a)
newTransmitterBy f = 
    do m <- newTVarIO Nothing
       return $ Transmitter f m

newTransmitter :: (Eq a) => IO (Transmitter a)
newTransmitter = newTransmitterBy (==)

transmit :: Transmitter a -> a -> IO (Message a)
transmit t a = atomically $
    do m_c <- readTVar $ transmitter_previous_message t
       case m_c of
           Just c | (transmitter_predicate t) (message_value c) a -> return c
           _ -> 
               do m <- send_ a
                  writeTVar (transmitter_previous_message t) $ Just m
                  return m

