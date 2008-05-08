\section{Driver}

\begin{code}
module Driver
    (DriverObject,
     driverNoop,
     newDriverObject,
     driverRead,
     driverGetAnswer,
     driverGetTable,
     driverAction)
    where

import Data.Maybe
import Control.Monad
import Data.IORef
import Data.List
import System.IO
import Tables
import RSAGL.Time
import Control.Concurrent

data RoguestarEngineState = RoguestarEngineState { 
    restate_tables :: [RoguestarTable], 
    restate_answers :: [(String,String)] }

data DriverData = DriverData {
	driver_engine_input_lines :: [String],
	driver_engine_input_line_fragment :: String,
	driver_engine_output_lines :: [String],
	driver_engine_state :: RoguestarEngineState,
	driver_dones :: Integer }

newtype DriverObject = DriverObject (IORef DriverData)

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
\end{code}

\begin{code}
driverNoop :: DriverObject -> IO ()
driverNoop driver_object = driverWrite driver_object "noop\n"
\end{code}

\subsection{Getting Protocol Answers}

\texttt{driverGetAnswer} retrieves an answer from the engine.  An answer is a single string.

\begin{code}
driverGetAnswer :: DriverObject -> String -> IO (Maybe String)
driverGetAnswer driver_object query =
    do driverQuery driver_object [query]
       driverGet driver_object (lookup query . restate_answers . driver_engine_state)
\end{code}

\subsection{Getting Protocol Tables}

\testtt{driverGetTable} retrieves a table from the engine.  These tables make up a kind of simple relational database.

\begin{code}
driverGetTable :: DriverObject -> String -> String -> IO (Maybe RoguestarTable)
driverGetTable driver_object the_table_name the_table_id =
    do driverQuery driver_object [the_table_name,the_table_id]
       driverGet driver_object (find (\x -> table_name x == the_table_name && table_id x == the_table_id) . restate_tables . driver_engine_state)
\end{code}

\texttt{driverQuery} transmits a read-only query against the state of the engine.

\begin{code}
driverQuery :: DriverObject -> [String] -> IO ()
driverQuery driver_object query = driverWrite driver_object $ "game query " ++ unwords query ++ "\n"
\end{code}

\begin{code}
driverAction :: DriverObject -> [String] -> IO ()
driverAction driver_object strs = 
    do modifyDriver driver_object $ \driver -> driver { driver_engine_state = RoguestarEngineState [] []}
       driverWrite driver_object $ "game action " ++ unwords strs ++ "\n"
\end{code}

\texttt{driverWrite} writes the specified command to standard output, automatically interleaving calls to read.
\texttt{driverWrite} will never write the same string twice between calls to \texttt{driverReset}.

\begin{code}
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
\end{code}

\texttt{maybeRead} reads one character if it is available.

\begin{code}
maybeRead :: IO (Maybe Char)
maybeRead = do ready <- hReady stdin
	       case ready of
			  False -> return Nothing
			  True -> do char <- getChar
				     return $ Just char
\end{code}

\texttt{driverRead} reads, parses, and stores data from the engine.

\begin{code}
driverRead :: DriverObject -> IO ()
driverRead driver_object = 
    do maybe_next_char <- maybeRead
       case maybe_next_char of
           Nothing -> return ()
           Just '\n' -> do modifyDriver driver_object
                               (\driver -> driver { driver_engine_input_lines= 
                                                    (reverse $ driver_engine_input_line_fragment driver) :
                                                    driver_engine_input_lines driver,
                                                    driver_engine_input_line_fragment=""});
                           driverUpdate driver_object
           Just next_char -> modifyDriver driver_object 
                                          (\driver -> driver { driver_engine_input_line_fragment=(next_char : 
                                                                                                  driver_engine_input_line_fragment driver) })
       when (isJust maybe_next_char) $ do --hPutChar stderr $ fromJust maybe_next_char -- uncomment to see everything we read in stderr
                                          driverRead driver_object
\end{code}

\texttt{driverUpdate} parses stored information in the driver, generating answers and tables.

\begin{code}
driverUpdate :: DriverObject -> IO ()
driverUpdate driver_object =
    do head_line <- driverGet driver_object (head . driver_engine_input_lines)
       when (head_line == "over") $ interpretText driver_object

data DriverInterpretationState = DINeutral
			       | DIScanningTable RoguestarTable
			       | DIError
				 deriving (Eq,Show)

interpretText :: DriverObject -> IO ()
interpretText driver_object = 
    do final_state <- foldM (interpretLine driver_object) DINeutral =<< driverGet driver_object (reverse . driver_engine_input_lines)
       when (final_state /= DINeutral) $ do hPutStr stderr "interpretText concluded in a non-neutral state, which was:"
					    hPutStr stderr (show final_state)
					    modifyDriver driver_object $ \driver -> initial_driver_data { 
					        driver_dones = driver_dones driver,
						driver_engine_state = driver_engine_state driver }
       modifyDriver driver_object $ \driver -> driver { driver_engine_input_lines = [] }

modifyEngineState :: DriverObject -> (RoguestarEngineState -> RoguestarEngineState) -> IO ()
modifyEngineState driver_object f = modifyDriver driver_object $ \driver -> driver { driver_engine_state = f $ driver_engine_state driver } 

interpretLine :: DriverObject -> DriverInterpretationState -> String -> IO DriverInterpretationState
interpretLine _ DIError _ = return DIError

interpretLine _ di_state "" = do return di_state -- ignore empty lines

interpretLine _ _ str | (head $ words str) == "protocol-error:" = 
    do hPutStr stderr str
       return DIError

interpretLine _ _ str | (head $ words str) == "error:" = 
    do hPutStr stderr str
       return DIError

interpretLine driver_object DINeutral "done" =
    do modifyDriver driver_object $ \driver -> driver { driver_engine_state = RoguestarEngineState [] [],
		   			                driver_engine_output_lines = [],
					                driver_dones = driver_dones driver + 1 }
       return DINeutral

interpretLine _ di_state "done" = 
    do hPutStr stderr  ("client-side protocol error: unexpected \"done\" in " ++ (show di_state) ++ " state.")
       return DIError

interpretLine _ DINeutral "over" = return DINeutral

interpretLine _ (DIScanningTable {}) "over" = 
    do hPutStr stderr "client-side protocol error: 'over' issued while reading a data table"
       return DIError

interpretLine driver_object DINeutral str | (head $ words str) == "answer:" && (length $ words str) == 3 =
    do modifyEngineState driver_object (\engine_state -> engine_state { restate_answers = (words str !! 1,words str !! 2):restate_answers engine_state })
       return DINeutral

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
    do hPutStr stderr "client-side protocol error: incomplete begin-table header"
       return DIError

interpretLine driver_object (DIScanningTable table) str | (head $ words str) == "end-table" =
  do modifyEngineState driver_object (\engine_state -> engine_state { restate_tables = (table { table_data=reverse $ table_data table}):restate_tables engine_state })
     return DINeutral

interpretLine _ (DIScanningTable table) str = 
    let table_row = words str
        in (if length table_row == (length $ table_header table)
	then return $ DIScanningTable (table { table_data = table_row : table_data table })
	else do hPutStr stderr "client-side protocol error: malformed table row"
		return DIError )

interpretLine _ _ str = 
    do hPutStr stderr str
       return DINeutral

\end{code}
