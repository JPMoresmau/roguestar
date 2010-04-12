{-# LANGUAGE OverloadedStrings #-}
-- | Interacts with Protocol module of roguestar-engine.
module Driver
    (DriverObject,RoguestarEngineState,FrozenDriver,
     freezeDriver,thawDriver,
     DriverClass(..),
     driverNoop,
     driverDones,
     newDriverObject,
     driverAction)
    where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Data.List as List
import Data.Map as Map
import System.IO
import Tables
import RSAGL.FRP.Time
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B

-- | Contains all of the information that is known about roguestar-engine at a specific moment in time.
--
data RoguestarEngineState = RoguestarEngineState { 
    restate_tables :: Map (B.ByteString,B.ByteString) RoguestarTable, -- index by table_name and table_id
    restate_answers :: Map B.ByteString B.ByteString }

-- | A class for DriverObjects.
class DriverClass a where
    -- | Retrieves an answer from the engine.  An answer is a single string.
    getAnswer :: a -> B.ByteString -> IO (Maybe B.ByteString)
    -- | Retrieves a table from the engine.  These tables make up a kind of simple relational database.
    getTable :: a -> B.ByteString -> B.ByteString -> IO (Maybe RoguestarTable)

instance DriverClass DriverObject where
    getAnswer driver_object query =
        do driverQuery driver_object [query]
           restate <- driverGet driver_object driver_engine_state
           getAnswer restate query
    getTable driver_object the_table_name the_table_id =
        do driverQuery driver_object [the_table_name,the_table_id]
           restate <- driverGet driver_object driver_engine_state
           getTable restate the_table_name the_table_id

instance DriverClass RoguestarEngineState where
    getAnswer restate query = return $ Map.lookup query $ restate_answers restate
    getTable restate the_table_name the_table_id = return $ (Map.lookup (the_table_name,the_table_id)) $ restate_tables restate

instance DriverClass FrozenDriver where
    getAnswer (FrozenDriver driver_object restate dones) query =
        do current_dones <- driverDones driver_object
           when (current_dones == dones) $ getAnswer driver_object query >> return ()
           getAnswer restate query
    getTable (FrozenDriver driver_object restate dones) the_table_name the_table_id =
        do current_dones <- driverDones driver_object
           when (current_dones == dones) $ getTable driver_object the_table_name the_table_id >> return ()
           getTable restate the_table_name the_table_id

freezeDriver :: (MonadIO m) => DriverObject -> m FrozenDriver
freezeDriver driver_object =
    do d <- liftIO $ driverGet driver_object id
       return $ FrozenDriver driver_object (driver_engine_state d) (driver_dones d)

thawDriver :: FrozenDriver -> DriverObject
thawDriver (FrozenDriver driver_object _ _) = driver_object

-- | Contains detailed information about character-by-character interaction with the engine.
data DriverData = DriverData {
        -- | Unparsed incomming information from the engine, previous lines, separated by line
	-- These lines are stores in reverse order from how they are recieved.
	driver_engine_input_lines :: [B.ByteString],
	-- | Lines already sent to the engine.  This is cleared whenever we send a
	-- "game action" line.  We retain this because it's wasteful to repeat queries
	-- when nothing has been committed back to the engine.
	driver_engine_output_lines :: [B.ByteString],
	-- | Parsed information about the game state.  This is cleared whenever we send a
	-- "game action" line.
	driver_engine_state :: RoguestarEngineState,
	-- | Count of the number of times we've recieved "done" from the engine.
	driver_dones :: Integer,
        -- | Count of the number of times we've sent "driver action" to the engine.
        driver_actions :: Integer,
        -- | Writer mutex
        driver_writing :: MVar (),
        -- | Reader mutex
        driver_reading :: MVar() }

-- | An object that contains the state of the interaction between the driver and "roguestar-engine".
-- Since the Driver interacts on 'stdin'/'stdout', there should probably only be one instance of this in any running program.
newtype DriverObject = DriverObject (MVar DriverData)

-- | A frozen 'DriverObject'.  It has a static view of the world as seen at the time that the Driver is fozen, however, the original
-- 'DriverObject' continues to be updated when this 'FrozenDriver' is used.
data FrozenDriver = FrozenDriver DriverObject RoguestarEngineState Integer

-- | Initialize a DriverObject.
newDriverObject :: IO DriverObject
newDriverObject = 
    do driver_object <- liftM DriverObject . newMVar =<< initialDriverData
       _ <- forkIO $ forever $ driverRead driver_object
       return driver_object

initialDriverData :: IO DriverData
initialDriverData = 
    do w <- newMVar ()
       r <- newMVar ()
       return $ DriverData {
           driver_engine_input_lines = [],
           driver_engine_output_lines = [],
           driver_engine_state = RoguestarEngineState Map.empty Map.empty,
           driver_actions = 0,
           driver_dones = 0,
           driver_writing = w,
           driver_reading = r }

driverGet :: DriverObject -> (DriverData -> a) -> IO a
driverGet (DriverObject mvar) f = liftM f $ readMVar mvar

modifyDriver :: DriverObject -> (DriverData -> DriverData) -> IO () 
modifyDriver (DriverObject mvar) f = modifyMVar_ mvar (return . f) >> return ()

-- | Specialized 'modifyDriver'.
modifyEngineState :: DriverObject -> (RoguestarEngineState -> RoguestarEngineState) -> IO ()
modifyEngineState driver_object f = modifyDriver driver_object $ \driver -> driver { driver_engine_state = f $ driver_engine_state driver } 

-- | Transmit a no-op to "roguestar-engine".
driverNoop :: DriverObject -> IO ()
driverNoop driver_object = driverWrite driver_object "noop\n"

-- | Total count of done messages received from the engine.
driverDones :: DriverObject -> IO Integer
driverDones driver_object = driverGet driver_object driver_dones

-- | Transmits a read-only query against the state of the engine.
driverQuery :: DriverObject -> [B.ByteString] -> IO ()
driverQuery driver_object query = driverWrite driver_object $ "game query " `B.append` B.unwords query `B.append` "\n"

-- | Commit an action to the engine.
driverAction :: DriverObject -> [B.ByteString] -> IO ()
driverAction driver_object strs = 
    do modifyDriver driver_object $ \driver -> driver { driver_engine_state = RoguestarEngineState Map.empty Map.empty,
                                                        driver_actions = succ $ driver_actions driver }
       driverWrite driver_object $ "game action " `B.append` B.unwords strs `B.append` "\n"

-- | Writes the specified command to standard output, will never write the same string twice between calls to 'driverReset'.
driverWrite :: DriverObject -> B.ByteString -> IO ()
driverWrite (DriverObject driver_mvar) str =
    do already_sent <- modifyMVar driver_mvar $ \driver ->
           do let already_sent = elem str $ driver_engine_output_lines driver
              return (if already_sent then driver else driver { driver_engine_output_lines = str:driver_engine_output_lines driver },already_sent)
       unless already_sent $ 
           do _ <- forkIO $ writing (DriverObject driver_mvar) $ B.putStr str >> hFlush stdout
              return ()

-- | Just read from the engine.  Whenever 'driverRead' reads an "over", it automatically
-- fires off 'interpretText' to parse the newly read information.
driverRead :: DriverObject -> IO ()
driverRead driver_object = reading driver_object $
    do str <- B.getLine
       modifyDriver driver_object (\driver -> driver { driver_engine_input_lines= str : driver_engine_input_lines driver })
       when (str == "over") $ interpretText driver_object

reading :: DriverObject -> IO a -> IO a
reading driver_object actionM = bracket (driverGet driver_object driver_reading >>= takeMVar) (const $ driverGet driver_object driver_reading >>= flip putMVar ()) (const actionM)

writing :: DriverObject -> IO a -> IO a
writing driver_object = bracket (driverGet driver_object driver_writing >>= takeMVar) (const $ driverGet driver_object driver_writing >>= flip putMVar ()) . const

{--------------------------------------------------------------------------------------------------
 -  This is the parser.
 - ------------------------------------------------------------------------------------------------}

data DriverInterpretationState = DINeutral
			       | DIScanningTable RoguestarTable
			       | DIError
				 deriving (Eq,Show)

-- | Parse 'driver_engine_input_lines'.
interpretText :: DriverObject -> IO ()
interpretText driver_object = 
    do final_state <- foldM (interpretLine driver_object) DINeutral =<< driverGet driver_object (reverse . driver_engine_input_lines)
       when (final_state /= DINeutral) $ do hPutStr stderr "interpretText concluded in a non-neutral state, which was:"
					    hPutStr stderr (show final_state)
					    modifyDriver driver_object $ \driver -> driver { 
                                                driver_engine_input_lines = [],
                                                driver_engine_output_lines = [] }
       modifyDriver driver_object $ \driver -> driver { driver_engine_input_lines = [] }

-- | 'interpretLine' is a simple line-by-line parser invoked using 'foldM'.
interpretLine :: DriverObject -> DriverInterpretationState -> B.ByteString -> IO DriverInterpretationState
interpretLine _ DIError _ = return DIError

-- Ignore empty lines.
interpretLine _ di_state "" = do return di_state

-- Report errors.  \"protocol-error\" means that engine believes that we screwed up.
-- \"error\" means that the engine admits it screwed up.
interpretLine _ _ str | (head $ B.words str) `elem` ["protocol-error:", "error:"] = 
    do B.hPutStr stderr str
       return DIError

-- Engine acknowledges completed database update.
interpretLine driver_object DINeutral "done" =
    do modifyDriver driver_object $ \driver -> driver { driver_engine_state = RoguestarEngineState Map.empty Map.empty,
		   			                driver_engine_output_lines = [],
					                driver_dones = driver_dones driver + 1 }
       return DINeutral

interpretLine _ di_state "done" = 
    do hPutStr stderr  ("Driver raised protocol error: unexpected \"done\" in " ++ (show di_state) ++ " state.")
       return DIError

-- Engine is finished answering a query or action.  (But it may continue to spew answers to other queries.)
interpretLine _ DINeutral "over" = return DINeutral

interpretLine _ (DIScanningTable {}) "over" = 
    do hPutStr stderr "Driver raised protocol error: 'over' issued while reading a data table"
       return DIError

-- Engine is answering a question.
interpretLine driver_object DINeutral str | (head $ B.words str) == "answer:" && (length $ B.words str) == 3 =
    do modifyEngineState driver_object $ \engine_state -> engine_state { restate_answers = Map.insert (B.words str !! 1) (B.words str !! 2) (restate_answers engine_state) }
       return DINeutral

-- Engine is opening a new table.
interpretLine _ DINeutral str | (head $ B.words str) == "begin-table" && (length $ B.words str) > 3 = 
    do t <- getTime
       let table_start_data = B.words str
       return $ DIScanningTable $ RoguestarTable {
						  table_created = t,
				                  table_name = table_start_data !! 1,
		                                  table_id = table_start_data !! 2,
		                                  table_header = drop 3 table_start_data,
                                                  table_data = []}

interpretLine _ _ str | (head $ B.words str) == "begin-table" =
    do hPutStr stderr "Driver raised protocol error: incomplete begin-table header"
       return DIError

-- Engine is closing a table.
interpretLine driver_object (DIScanningTable table) str | (head $ B.words str) == "end-table" =
  do modifyEngineState driver_object $ \engine_state -> engine_state { restate_tables = Map.insert (table_name table,table_id table) 
                                                                                        (table { table_data = reverse $ table_data table }) $ restate_tables engine_state }
     forceTable table
     return DINeutral

-- Inside an open table, read a single row of that table.
interpretLine _ (DIScanningTable table) str = 
    let table_row = B.words str
        in (if length table_row == (length $ table_header table)
	then return $ DIScanningTable (table { table_data = table_row : table_data table })
	else do hPutStr stderr "Driver raised protocol error: malformed table row"
		return DIError )

-- If we don't know what else to do, just print to stderr.
interpretLine _ _ str = 
    do B.hPutStr stderr str
       return DINeutral
