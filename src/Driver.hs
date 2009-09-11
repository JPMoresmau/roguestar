-- | Interacts with Protocol module of roguestar-engine.
module Driver
    (DriverObject,RoguestarEngineState,FrozenDriver,
     freezeDriver,thawDriver,
     DriverClass(..),
     driverNoop,
     driverDones,
     newDriverObject,
     driverRead,
     driverAction)
    where

import Data.Maybe
import Data.IORef
import Data.List
import System.IO
import Tables
import RSAGL.FRP.Time
import Control.Monad.Reader

-- | Contains all of the information that is known about roguestar-engine at a specific moment in time.
--
data RoguestarEngineState = RoguestarEngineState { 
    restate_tables :: [RoguestarTable],
    restate_answers :: [(String,String)] }

-- | A class for DriverObjects.
class DriverClass a where
    -- | Retrieves an answer from the engine.  An answer is a single string.
    getAnswer :: a -> String -> IO (Maybe String)
    -- | Retrieves a table from the engine.  These tables make up a kind of simple relational database.
    getTable :: a -> String -> String -> IO (Maybe RoguestarTable)

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
    getAnswer restate query = return $ lookup query $ restate_answers restate
    getTable restate the_table_name the_table_id = return $ (find (\x -> table_name x == the_table_name && table_id x == the_table_id)) $ restate_tables restate

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
    do restate <- liftIO $ driverGet driver_object driver_engine_state
       dones <- liftIO $ driverDones driver_object
       return $ FrozenDriver driver_object restate dones

thawDriver :: FrozenDriver -> DriverObject
thawDriver (FrozenDriver driver_object _ _) = driver_object

-- | Contains detailed information about character-by-character interaction with the engine.
data DriverData = DriverData {
        -- | Unparsed incomming information from the engine, previous lines, separated by line
	-- These lines are stores in reverse order from how they are recieved.
	driver_engine_input_lines :: [String],
	-- | Unparsed incomming information from the engine, current line fragment
	-- This is cleared when the current line is completed, and moved into driver_engine_input_lines
	driver_engine_input_line_fragment :: String,
	-- | Lines already sent to the engine.  This is cleared whenever we send a
	-- "game action" line.  We retain this because it's wasteful to repeat queries
	-- when nothing has been committed back to the engine.
	driver_engine_output_lines :: [String],
	-- | Parsed information about the game state.  This is cleared whenever we send a
	-- "game action" line.
	driver_engine_state :: RoguestarEngineState,
	-- | Count of the number of time we've recieved "done" from the engine.
	driver_dones :: Integer }

-- | An object that contains the state of the interaction between the driver and "roguestar-engine".
-- Since the Driver interacts on 'stdin'/'stdout', there should probably only be one instance of this in any running program.
newtype DriverObject = DriverObject (IORef DriverData)

-- | A frozen 'DriverObject'.  It has a static view of the world as seen at the time that the Driver is fozen, however, the original
-- 'DriverObject' continues to be updated when this 'FrozenDriver' is used.
data FrozenDriver = FrozenDriver DriverObject RoguestarEngineState Integer

-- | Initialize a DriverObject.
newDriverObject :: IO DriverObject
newDriverObject = liftM DriverObject $ newIORef initial_driver_data

initial_driver_data :: DriverData
initial_driver_data = DriverData {
    driver_engine_input_lines = [],
    driver_engine_input_line_fragment = [],
    driver_engine_output_lines = [],
    driver_engine_state = RoguestarEngineState [] [],
    driver_dones = 0 }

driverGet :: DriverObject -> (DriverData -> a) -> IO a
driverGet (DriverObject ioref) f = liftM f $ readIORef ioref

modifyDriver :: DriverObject -> (DriverData -> DriverData) -> IO () 
modifyDriver (DriverObject ioref) f = modifyIORef ioref f

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
driverQuery :: DriverObject -> [String] -> IO ()
driverQuery driver_object query = driverWrite driver_object $ "game query " ++ unwords query ++ "\n"

-- | Commit an action to the engine.
driverAction :: DriverObject -> [String] -> IO ()
driverAction driver_object strs = 
    do modifyDriver driver_object $ \driver -> driver { driver_engine_state = RoguestarEngineState [] []}
       driverWrite driver_object $ "game action " ++ unwords strs ++ "\n"

-- | Writes the specified command to standard output, automatically interleaving calls to read.
-- will never write the same string twice between calls to 'driverReset'.
driverWrite :: DriverObject -> String -> IO ()
driverWrite driver_object str = 
    do already_sent <- driverGet driver_object (elem str . driver_engine_output_lines)
       unless already_sent $
           do modifyDriver driver_object $ \driver -> driver { driver_engine_output_lines=str:driver_engine_output_lines driver }
              driverWrite_ driver_object str
       driverRead driver_object

driverWrite_ :: DriverObject -> String -> IO ()
driverWrite_ driver_object "" = 
    do hFlush stdout
       driverRead driver_object
driverWrite_ driver_object str = 
    do driverRead driver_object
       putChar $ head str
       --hPutChar stderr $ head str -- uncomment to see everything we write in stderr
       driverWrite_ driver_object $ tail str

-- | Reads one character if it is available.
maybeRead :: IO (Maybe Char)
maybeRead = do ready <- hReady stdin
	       case ready of
			  False -> return Nothing
			  True -> do char <- getChar
				     return $ Just char

-- | Just read from the engine.  Whenever 'driverRead' reads an "over", it automatically
-- fires off 'interpretText' to parse the newly read information.
driverRead :: DriverObject -> IO ()
driverRead driver_object = 
    do maybe_next_char <- maybeRead
       case maybe_next_char of
           Nothing -> return ()
           Just '\n' -> 
	       do modifyDriver driver_object (\driver -> driver { driver_engine_input_lines= 
                                                 (reverse $ driver_engine_input_line_fragment driver) : driver_engine_input_lines driver,
                                                 driver_engine_input_line_fragment=""})
                  input_lines <- driverGet driver_object driver_engine_input_lines
                  case input_lines of
                      ("over":_) -> interpretText driver_object
                      _ -> return ()
           Just next_char -> modifyDriver driver_object (\driver -> driver { 
	       driver_engine_input_line_fragment=(next_char : driver_engine_input_line_fragment driver) })
       when (isJust maybe_next_char) $ do --hPutChar stderr $ fromJust maybe_next_char -- uncomment to see everything we read in stderr
                                          driverRead driver_object

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
					    modifyDriver driver_object $ \driver -> initial_driver_data { 
					        driver_dones = driver_dones driver,
						driver_engine_state = driver_engine_state driver }
       modifyDriver driver_object $ \driver -> driver { driver_engine_input_lines = [] }

-- | 'interpretLine' is a simple line-by-line parser invoked using 'foldM'.
interpretLine :: DriverObject -> DriverInterpretationState -> String -> IO DriverInterpretationState
interpretLine _ DIError _ = return DIError

-- Ignore empty lines.
interpretLine _ di_state "" = do return di_state

-- Report errors.  \"protocol-error\" means that engine believes that we screwed up.
-- \"error\" means that the engine admits it screwed up.
interpretLine _ _ str | (head $ words str) `elem` ["protocol-error:", "error:"] = 
    do hPutStr stderr str
       return DIError

-- Engine acknowledges completed database update.
interpretLine driver_object DINeutral "done" =
    do modifyDriver driver_object $ \driver -> driver { driver_engine_state = RoguestarEngineState [] [],
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
interpretLine driver_object DINeutral str | (head $ words str) == "answer:" && (length $ words str) == 3 =
    do modifyEngineState driver_object (\engine_state -> engine_state { restate_answers = (words str !! 1,words str !! 2):restate_answers engine_state })
       return DINeutral

-- Engine is opening a new table.
interpretLine _ DINeutral str | (head $ words str) == "begin-table" && (length $ words str) > 3 = 
    do t <- getTime
       let table_start_data = words str
       return $ DIScanningTable $ RoguestarTable {
						  table_created = t,
				                  table_name = table_start_data !! 1,
		                                  table_id = table_start_data !! 2,
		                                  table_header = drop 3 table_start_data,
                                                  table_data = []}

interpretLine _ _ str | (head $ words str) == "begin-table" =
    do hPutStr stderr "Driver raised protocol error: incomplete begin-table header"
       return DIError

-- Engine is closing a table.
interpretLine driver_object (DIScanningTable table) str | (head $ words str) == "end-table" =
  do modifyEngineState driver_object (\engine_state -> engine_state { restate_tables = (table { table_data=reverse $ table_data table}):restate_tables engine_state })
     return DINeutral

-- Inside an open table, read a single row of that table.
interpretLine _ (DIScanningTable table) str = 
    let table_row = words str
        in (if length table_row == (length $ table_header table)
	then return $ DIScanningTable (table { table_data = table_row : table_data table })
	else do hPutStr stderr "Driver raised protocol error: malformed table row"
		return DIError )

-- If we don't know what else to do, just print crap to stderr.
interpretLine _ _ str = 
    do hPutStr stderr str
       return DINeutral
