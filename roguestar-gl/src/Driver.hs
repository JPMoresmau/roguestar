{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-- | Interacts with Protocol module of roguestar-engine.
module Driver
    (DriverObject,RoguestarEngineState,FrozenDriver,
     freezeDriver,thawDriver,
     DriverClass(..),
     driverSendError,
     driverNoop,
     driverDones,
     newDriverObject,
     driverAction)
    where

import Control.Concurrent.STM
import Control.Concurrent
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe
import System.IO
import Tables
import RSAGL.FRP.Time
import Control.Applicative
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B

-- | Contains detailed information about ongoing interaction with the engine.
data DriverObject = DriverObject {
        -- | Unparsed incomming information from the engine.  These lines are
        -- stored in reverse order from how they were recieved.
        driver_incomming :: TVar [B.ByteString],
        -- | Unsent outgoing information to the driver.  Either a destructive
        -- action or a lossy query.
        driver_outgoing :: TVar (Either B.ByteString [B.ByteString]),
        -- | Unsent outgoing error messages.  Stored in reverse order.
        driver_errors :: TVar [B.ByteString],
        -- | Lines already sent to the engine.  This is cleared whenever we send
        -- a "game action" line.  We retain this because it's wasteful to repeat
        -- queries when nothing has been committed back to the engine.
        driver_cached_lines :: TVar (Set.Set [B.ByteString]),
        -- | Parsed information about the game state.  This is cleared whenever
        -- we send a "game action" line.
        driver_engine_state :: TVar RoguestarEngineState,
        -- | Count of the number of times we've recieved "done" from the engine.
        driver_dones :: TVar Integer,
        -- | Count of the number of destructive actions we've sent to the
        -- engine.
        driver_actions :: TVar Integer,
        -- | Current time, updated constantly by the read/write/error loops.
        driver_time :: TVar Time }

-- | A frozen 'DriverObject'.  It has a static view of the world as seen at the
-- time that the Driver is fozen, however, the original 'DriverObject' continues
-- to be updated when this 'FrozenDriver' is used.
data FrozenDriver = FrozenDriver {
    _frozen_driver :: DriverObject,
    _frozen_state :: RoguestarEngineState,
    _frozen_actions :: Integer,
    _frozen_dones :: Integer }

-- | Contains all of the information that is known about roguestar-engine at a
-- specific moment in time.
data RoguestarEngineState = RoguestarEngineState {
    -- index by (table_name, table_id)
    restate_tables :: Map (B.ByteString,B.ByteString) RoguestarTable,
    restate_answers :: Map B.ByteString B.ByteString }

-- | Initialize a DriverObject.  Call only once in any given process.
newDriverObject :: IO DriverObject
newDriverObject =
    do driver_object <- initialDriverData
       _ <- forkIO $ forever $ driverReadLoop driver_object
       _ <- forkIO $ forever $ driverWriteLoop driver_object
       _ <- forkIO $ forever $ driverErrorLoop driver_object
       return driver_object

initialDriverData :: IO DriverObject
initialDriverData = DriverObject <$>
    newTVarIO [] <*>
    newTVarIO (Right []) <*>
    newTVarIO [] <*>
    newTVarIO Set.empty <*>
    newTVarIO (RoguestarEngineState Map.empty Map.empty) <*>
    newTVarIO 0 <*>
    newTVarIO 0 <*>
    (newTVarIO =<< getTime)

updateTime :: DriverObject -> Time -> STM ()
updateTime driver_object t =
    do old_time <- readTVar (driver_time driver_object)
       when (t > old_time) $
           writeTVar (driver_time driver_object) t

-- | Just write to the engine.
driverWriteLoop :: DriverObject -> IO ()
driverWriteLoop driver_object =
    do hFlush stdout
       (mapM_ B.putStrLn =<<) $ atomically $
           do writes <- readTVar (driver_outgoing driver_object)
              writeTVar (driver_outgoing driver_object) $ Right []
              case writes of
                  Left destructive ->
                      do writeTVar (driver_cached_lines driver_object) Set.empty
                         writeTVar (driver_actions driver_object) . succ =<<
                             readTVar (driver_actions driver_object)
                         return [destructive]
                  Right queries ->
                      do when (List.null queries) retry
                         return $ reverse queries

driverErrorLoop :: DriverObject -> IO ()
driverErrorLoop driver_object =
    do hFlush stderr
       (mapM_ (B.hPutStrLn stderr) =<<) $ atomically $
           do errs <- readTVar (driver_errors driver_object)
              when (List.null errs) retry
              writeTVar (driver_errors driver_object) []
              return $ reverse errs

-- | Just read from the engine.  Whenever 'driverRead' reads an "over", it
-- automatically fires off 'interpretText' to parse the newly read information.
driverReadLoop :: DriverObject -> IO ()
driverReadLoop driver_object =
    do str <- B.getLine
       t <- getTime
       atomically $
           do updateTime driver_object t
              readses <- readTVar (driver_incomming driver_object)
              writeTVar (driver_incomming driver_object) $ str : readses
              when (str == "over") $ interpretText driver_object

-- | A class for DriverObjects.
class DriverClass a where
    -- | Retrieves an answer from the engine.  An answer is a single string.
    getAnswer :: a -> B.ByteString -> STM (Maybe B.ByteString)
    -- | Retrieves a table from the engine.  These tables make up a kind of
    -- simple relational database.
    getTable :: a -> B.ByteString -> B.ByteString -> STM (Maybe RoguestarTable)

instance DriverClass DriverObject where
    getAnswer driver_object query =
        do driverQuery driver_object [query]
           restate <- readTVar (driver_engine_state driver_object)
           getAnswer restate query
    getTable driver_object the_table_name the_table_id =
        do driverQuery driver_object [the_table_name,the_table_id]
           restate <- readTVar (driver_engine_state driver_object)
           getTable restate the_table_name the_table_id

instance DriverClass RoguestarEngineState where
    getAnswer restate query = return $
        Map.lookup query $ restate_answers restate
    getTable restate the_table_name the_table_id = return $
        (Map.lookup (the_table_name,the_table_id)) $ restate_tables restate

instance DriverClass FrozenDriver where
    getAnswer (FrozenDriver driver_object restate acts _) query =
        do result <- getAnswer restate query
           when (isNothing result) $
               do current_actions <- readTVar (driver_actions driver_object)
                  when (current_actions == acts) $
                      getAnswer driver_object query >> return ()
           return result
    getTable (FrozenDriver driver_object restate acts _)
             the_table_name the_table_id =
        do result <- getTable restate the_table_name the_table_id
           when (isNothing result) $
               do current_actions <- readTVar (driver_actions driver_object)
                  when (current_actions == acts) $
                      getTable driver_object the_table_name the_table_id >>
                          return ()
           return result

-- | Transmits a read-only query against the state of the engine.
driverSendLossy :: DriverObject -> [B.ByteString] -> STM ()
driverSendLossy driver_object query =
    do cached_lines <- readTVar (driver_cached_lines driver_object)
       when (not $ query `Set.member` cached_lines) $
           do writes <- either (const retry) return =<<
                  readTVar (driver_outgoing driver_object)
              writeTVar (driver_outgoing driver_object) $
                  Right $ B.unwords query : writes
              writeTVar (driver_cached_lines driver_object) $
                  Set.insert query cached_lines

-- | Commit a destructive action to the engine.
driverSendDestructive :: DriverObject ->
                     [B.ByteString] ->
                     STM ()
driverSendDestructive driver_object strs =
    do _ <- either (const retry) return =<<
           readTVar (driver_outgoing driver_object)
       writeTVar (driver_engine_state driver_object) $
           RoguestarEngineState Map.empty Map.empty
       writeTVar (driver_outgoing driver_object) $ Left $
           B.unwords strs

-- | Transmits a read-only query against the state of the engine.
driverSendError :: DriverObject -> B.ByteString -> STM ()
driverSendError driver_object err =
    writeTVar (driver_errors driver_object) . (err:) =<<
        readTVar (driver_errors driver_object)

-- | Get an immutable driver.  This driver still echoes requests
-- to it's parent driver.
freezeDriver :: DriverObject -> STM FrozenDriver
freezeDriver driver_object =
    do restate <- readTVar (driver_engine_state driver_object)
       actions <- readTVar (driver_actions driver_object)
       dones <- readTVar (driver_dones driver_object)
       return $ FrozenDriver driver_object restate actions dones

-- | Not strictly necessary, you can use the original.
thawDriver :: FrozenDriver -> DriverObject
thawDriver (FrozenDriver driver_object _ _ _) = driver_object

-- | Number of times 'done' has been received from the engine.
driverDones :: FrozenDriver -> STM Integer
driverDones (FrozenDriver _ _ _ dones) = return dones

-- | Issue a \"driver query\" to the engine.
driverQuery :: DriverObject -> [B.ByteString] -> STM ()
driverQuery driver_object query = driverSendLossy driver_object $
                                      "game":"query":query

-- | Commit a \"driver action\" to the engine.
driverAction :: DriverObject -> [B.ByteString] -> STM ()
driverAction driver_object query = driverSendDestructive driver_object $
                                      "game":"action":query

-- | Issue a no-op to the engine..
driverNoop :: DriverObject -> STM ()
driverNoop driver_object = driverSendDestructive driver_object ["noop"]

{-------------------------------------------------------------------------------
 -  This is the parser.
 - ----------------------------------------------------------------------------}

data DriverInterpretationState = DINeutral
                               | DIScanningTable RoguestarTable
                               | DIError
                               deriving (Eq,Show)

-- | Parse 'driver_engine_input_lines'.
interpretText :: DriverObject -> STM ()
interpretText driver_object =
    do final_state <- foldM (interpretLine driver_object) DINeutral =<<
           liftM reverse (readTVar $ driver_incomming driver_object)
       when (final_state /= DINeutral) $
           do driverSendError driver_object
                  "interpretText concluded in a non-neutral state, which was:"
              driverSendError driver_object $ B.pack $ show final_state
       writeTVar (driver_incomming driver_object) []

-- | 'interpretLine' is a simple line-by-line parser invoked using 'foldM'.
interpretLine :: DriverObject ->
                 DriverInterpretationState ->
                 B.ByteString ->
                 STM DriverInterpretationState
interpretLine _ DIError _ = return DIError

-- Ignore empty lines.
interpretLine _ di_state str | List.null (B.words str) = do return di_state

-- Report errors.  \"protocol-error\" means that engine believes that we screwed
-- up.  \"error\" means that the engine admits it screwed up.
interpretLine driver_object _ str | (head $ B.words str) `elem`
                                    ["protocol-error:", "error:"] =
    do driverSendError driver_object str
       return DIError

-- Engine acknowledges completed database update.
interpretLine driver_object DINeutral "done" =
    do writeTVar (driver_engine_state driver_object) $
           RoguestarEngineState Map.empty Map.empty
       writeTVar (driver_dones driver_object) . succ =<<
           readTVar (driver_dones driver_object)
       return DINeutral

interpretLine driver_object di_state "done" =
    do driverSendError driver_object $
           "Driver raised protocol error: unexpected \"done\" in " `B.append`
               (B.pack $ show di_state) `B.append` " state."
       return DIError

-- Engine is finished answering a query or action.
-- (But it may continue to spew answers to other queries.)
interpretLine _ DINeutral "over" = return DINeutral

interpretLine driver_object (DIScanningTable {}) "over" =
    do driverSendError driver_object $
           "Driver raised protocol error: 'over' issued while reading a data table"
       return DIError

-- Engine is answering a question.
interpretLine driver_object DINeutral str
        | ["answer:",key,value] <- B.words str =
    do engine_state <- readTVar (driver_engine_state driver_object)
       writeTVar (driver_engine_state driver_object) $
           engine_state {
               restate_answers = Map.insert key value
                                 (restate_answers engine_state) }
       return DINeutral

-- Engine is opening a new table.
interpretLine driver_object DINeutral str
        | ("begin-table":tname:tid:theaders) <- B.words str
        , not (List.null theaders) =
    do t <- readTVar (driver_time driver_object)
       return $ DIScanningTable $ RoguestarTable {
           table_created = t,
           table_name = tname,
           table_id = tid,
           table_header = theaders,
           table_data = [] }

interpretLine driver_object _ str | (head $ B.words str) == "begin-table" =
    do driverSendError driver_object
           "Driver raised protocol error: incomplete begin-table header"
       return DIError

-- Engine is closing a table.
interpretLine driver_object (DIScanningTable table) str
        | (head $ B.words str) == "end-table" =
    do engine_state <- readTVar (driver_engine_state driver_object)
       writeTVar (driver_engine_state driver_object) $
           engine_state { restate_tables =
               Map.insert (table_name table, table_id table)
                   (table { table_data = reverse $ table_data table }) $
                       restate_tables engine_state }
       return DINeutral

-- Inside an open table, read a single row of that table.
interpretLine driver_object (DIScanningTable table) str =
    let table_row = B.words str
        in (if length table_row == (length $ table_header table)
        then return $ DIScanningTable (table {
                 table_data = table_row : table_data table })
        else do driverSendError driver_object
                    "Driver raised protocol error: malformed table row"
                return DIError )

-- If we don't know what else to do, just print to stderr.
interpretLine driver_object _ str =
    do driverSendError driver_object str
       return DINeutral

