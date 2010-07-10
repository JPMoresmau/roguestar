{-# LANGUAGE Arrows #-}

-- | A memoization scheme in which a piece of information is tagged with
-- a unique identifier for its source.  Messages can be combined,
-- and the tagging information indicates the specific combination.
-- On the receiving end, we memoize the single most recent incoming
-- message, and reuse it if the source information matches.
--
module RSAGL.FRP.Message
    (Message,
     consistent,
     consistency,
     peek,
     Transmitter,
     newTransmitterBy,
     newTransmitter,
     Receiver,
     newReceiver,
     (<<*>>),
     send,
     receive,
     transmit)
    where

import System.IO.Unsafe
import Control.Concurrent.STM

-- | A sourced packet of information.
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

-- | A unique tag with fast comparison.
data Source =
    Source Integer
  | Apply Source Source
    deriving (Eq,Ord)

data Builtin = Nest deriving (Eq,Ord)

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
    message_source = Apply (message_source f)
                           (message_source k),
    message_value = message_value f $ message_value k }

-- | An object that can memoize sequentially matching incoming messages.
data Receiver a = Receiver {
    receiver_previous_message :: TVar (Maybe (Message a)) }

newReceiver :: IO (Receiver a)
newReceiver =
    do m <- newTVarIO Nothing
       return $ Receiver m

-- | Memoizes an incomming message stream.
receive :: Receiver a -> Message a -> IO a
receive r m = atomically $
    do m_c <- readTVar $ receiver_previous_message r
       case m_c of
          Just c | message_source c == message_source m ->
              return $ message_value c
          _ ->
              do writeTVar (receiver_previous_message r) $ Just m
                 return $ message_value m

-- | An object that can memoize matching sequential outgoing messages.
data Transmitter a = Transmitter {
    transmitter_predicate :: a -> a -> Bool,
    transmitter_previous_message :: TVar (Maybe (Message a)) }

-- Defines a 'Transmitter' that uses a custom predicate to identify
-- matching outgoing messages.  The parameters of the predicate
-- are the cached value and the new value, respectively.
newTransmitterBy :: (a -> a -> Bool) -> IO (Transmitter a)
newTransmitterBy f =
    do m <- newTVarIO Nothing
       return $ Transmitter f m

-- | Equivalent to @newTransmitterBy (==)@.
newTransmitter :: (Eq a) => IO (Transmitter a)
newTransmitter = newTransmitterBy (==)

-- | Tags an outgoing stream for memoization.
transmit :: Transmitter a -> a -> IO (Message a)
transmit t a = atomically $
    do m_c <- readTVar $ transmitter_previous_message t
       case m_c of
           Just c | (transmitter_predicate t) (message_value c) a -> return c
           _ ->
               do m <- send_ a
                  writeTVar (transmitter_previous_message t) $ Just m
                  return m

