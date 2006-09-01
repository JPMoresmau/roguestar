module Driver
    (driverRead,
     driverRequestAnswer,
     driverRequestTable,
     driverAction)
    where

import Control.Monad
import Data.IORef
import Data.List
import System.IO
import Globals
import PrintText
import Tables

-- |
-- driverRequestAnswer globals_ref "why", sends "game query why" to the
-- engine and answers the result, or Nothing if no result is forthcomming.
--
driverRequestAnswer :: IORef RoguestarGlobals -> String -> IO (Maybe String)
driverRequestAnswer _ question | (length $ words question) /= 1 = error ("driverRequestAnswer: 'question' must be a single word (" ++ question ++ ")")
driverRequestAnswer globals_ref question = 
    do globals <- readIORef globals_ref
       let answer = lookup question $ restate_answers $ global_engine_state globals
	   in case answer of
			  Nothing -> do driverWrite globals_ref ("game query " ++ question ++ "\n")
				        return Nothing
			  Just just_answer -> return $ Just just_answer

driverRequestTable :: IORef RoguestarGlobals -> String -> String -> IO (Maybe RoguestarTable)
driverRequestTable globals_ref the_table_name the_table_id =
    do globals <- readIORef globals_ref
       let table = find (\x -> table_name x == the_table_name && table_id x == the_table_id) $ restate_tables $ global_engine_state globals 
	   in case table of
			 Nothing -> do driverWrite globals_ref ("game query " ++ the_table_name ++ "\n")
				       return Nothing
			 Just just_table -> return $ Just just_table

driverAction :: IORef RoguestarGlobals -> [String] -> IO ()
driverAction globals_ref strs = driverWrite globals_ref ("game action " ++ (unwords strs) ++ "\n")

-- |
-- Writes the specified command to standard output, automatically triggering a read, in parallel.
-- driverWrite will never write the same string twice between calls to driverReset,
-- but it will ensure that a read attempt occurs no matter what.
--
driverWrite :: IORef RoguestarGlobals -> String -> IO ()
driverWrite globals_ref str = do globals <- readIORef globals_ref
				 when (not $ elem str $ global_engine_output_lines globals) $ 
				      do writeIORef globals_ref $ globals { global_engine_output_lines=str:global_engine_output_lines globals }
					 driverWrite_ globals_ref str
				 driverRead globals_ref --extra read in case write never happened (harmless)

driverWrite_ :: IORef RoguestarGlobals -> String -> IO ()
driverWrite_ globals_ref "" = do hFlush stdout
				 driverRead globals_ref
driverWrite_ globals_ref str = do driverRead globals_ref
				  putChar $ head str
				  driverWrite_ globals_ref $ tail str

maybeRead :: IO (Maybe Char)
maybeRead = do ready <- hReady stdin
	       case ready of
			  False -> return Nothing
			  True -> do char <- getChar
				     return $ Just char

driverRead :: IORef RoguestarGlobals -> IO ()
driverRead globals_ref = 
    do globals0 <- readIORef globals_ref
       maybe_next_char <- maybeRead
       case maybe_next_char of
			    Nothing -> return ()
			    Just next_char -> do (if next_char == '\n'
						  then do {
							   writeIORef globals_ref (globals0 { global_engine_input_lines=(reverse $ global_engine_input_line_fragment globals0) : global_engine_input_lines globals0,
											      global_engine_input_line_fragment=""});
							   driverUpdate globals_ref
							  }
						  else writeIORef globals_ref (globals0 { global_engine_input_line_fragment=(next_char : global_engine_input_line_fragment globals0) }))
						 driverRead globals_ref

driverUpdate :: IORef RoguestarGlobals -> IO ()
driverUpdate globals_ref = 
    do globals <- readIORef globals_ref
       when (head (global_engine_input_lines globals) == "over") $ interpretText globals_ref

data DriverInterpretationState = DINeutral
			       | DIScanningTable RoguestarTable
			       | DIError
				 deriving (Eq,Show)

interpretText :: IORef RoguestarGlobals -> IO ()
interpretText globals_ref = 
    do globals0 <- readIORef globals_ref
       final_state <- foldM (interpretLine globals_ref) DINeutral $ reverse $ global_engine_input_lines globals0
       when (final_state /= DINeutral) $ do printText globals_ref Untranslated "interpretText concluded in a non-neutral state, which was:"
					    printText globals_ref Untranslated (show final_state)
       globals1 <- readIORef globals_ref
       writeIORef globals_ref $ globals1 { global_engine_input_lines = [] }

interpretLine :: IORef RoguestarGlobals -> DriverInterpretationState -> String -> IO DriverInterpretationState
interpretLine _ DIError _ = return DIError

interpretLine _ di_state "" = do return di_state -- ignore empty lines

interpretLine globals_ref _ str | (head $ words str) == "protocol-error:" = 
				    do printText globals_ref Untranslated str
				       return DIError

interpretLine globals_ref _ str | (head $ words str) == "error:" = 
				    do printText globals_ref Untranslated str
				       return DIError

interpretLine globals_ref DINeutral "done" =
    do globals <- readIORef globals_ref
       writeIORef globals_ref $ globals { global_engine_state = global_engine_state roguestar_globals_0,
					  global_engine_output_lines = global_engine_output_lines roguestar_globals_0,
					  global_dones = global_dones globals + 1 }
       return DINeutral

interpretLine globals_ref distate "done" = 
    do printText globals_ref Untranslated ("gui-side protocol error: unexpected \"done\" in " ++ (show distate) ++ " state.")
       return DIError

interpretLine _ DINeutral "over" = return DINeutral

interpretLine globals_ref (DIScanningTable {}) "over" = 
    do printText globals_ref Untranslated "gui-side protocol error: 'over' issued while reading a data table"
       return DIError

interpretLine globals_ref DINeutral str | (head $ words str) == "answer:" && (length $ words str) == 3 =
					    do globals <- readIORef globals_ref
					       let engine_state0 = global_engine_state globals
						   answers0 = restate_answers engine_state0
						   answers' = (words str !! 1,words str !! 2):answers0
						   engine_state' = engine_state0 { restate_answers = answers' }
					       writeIORef globals_ref $ globals { global_engine_state = engine_state' }
					       return DINeutral

interpretLine globals_ref DINeutral str | (head $ words str) == "begin-table" = 
					    let table_start_data = words str
						in (if length table_start_data > 3
						    then return $ DIScanningTable $ RoguestarTable {
												    table_name = table_start_data !! 1,
												    table_id = table_start_data !! 2,
												    table_header = drop 3 table_start_data,
												    table_data = []}
						    else do { printText globals_ref Untranslated "gui-side protocol error: incomplete begin-table header"; 
							      return DIError })

interpretLine globals_ref (DIScanningTable table) "end-table" =
    do globals <- readIORef globals_ref
       let engine_state0 = global_engine_state globals
	   tables0 = restate_tables engine_state0
	   tables' = (table {table_data=reverse $ table_data table}):tables0
	   engine_state' = engine_state0 { restate_tables=tables' }
	   in writeIORef globals_ref $ globals { global_engine_state = engine_state' }
       return DINeutral

interpretLine globals_ref (DIScanningTable table) str = let table_row = words str
							    in (if length table_row == (length $ table_header table)
								then return $ DIScanningTable (table { table_data = table_row : table_data table })
								else do { printText globals_ref Untranslated "gui-side protocol error: malformed table row";
									  return DIError })

interpretLine globals_ref _ str = do printText globals_ref Untranslated (str)
				     return DINeutral