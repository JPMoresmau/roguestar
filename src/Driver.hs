--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module Driver
    (DataFreshness(..),
     driverNoop,
     driverRead,
     driverGetAnswer,
     driverRequestAnswer,
     driverGetTable,
     driverRequestTable,
     driverAction)
    where

import Data.Maybe
import Control.Monad
import Data.IORef
import Data.List
import System.IO
import Globals
import PrintText
import Tables
import Time

-- |
-- [@Fresh@] guarantees that the data returned will be exactly the data that is in the engine, but this data is less likely to be immediately available; send a driverRequest* is necessary.
-- [@New@] return new data, but the engine might have changed since then; send a driverRequest* is necessary.
-- [@Old@] return data that is at least one turn old, if the data isn't available there is no way to request it from the engine
-- [@Anything@] return new data if available, otherwise old data
--
data DataFreshness = Fresh | New | Old | Anything deriving (Show,Eq)

driverNoop :: IORef RoguestarGlobals -> IO ()
driverNoop globals_ref = driverWrite globals_ref "noop\n"

-- |
-- driverGetAnswer, as driverRequestAnswer, but if data of the specified freshness is not
-- available, the result will be Nothing.
--
driverGetAnswer :: IORef RoguestarGlobals -> DataFreshness -> String -> IO (Maybe String)
driverGetAnswer globals_ref Fresh question = 
    do stale <- liftM global_stale $ readIORef globals_ref
       if stale then return Nothing else driverRequestAnswer globals_ref question
driverGetAnswer globals_ref New question = driverRequestAnswer globals_ref question
driverGetAnswer globals_ref Old question = liftM (lookup question . restate_answers . global_old_engine_state) $ readIORef globals_ref
driverGetAnswer globals_ref Anything question =
    do new <- driverGetAnswer globals_ref New question
       old <- driverGetAnswer globals_ref Old question
       return $ listToMaybe $ catMaybes [new,old]
       

-- |
-- Retrives a piece of data from the engine, using a "game query ..." statement and
-- parsing any "answer: ..." statement that results.
--
-- If the engine doesn't respond immediately, returns Nothing.
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

-- |
-- As driverRequestTable, but if data of the specified freshness is not available, the
-- result will be Nothing.
--
driverGetTable :: IORef RoguestarGlobals -> DataFreshness -> String -> String -> IO (Maybe RoguestarTable)
driverGetTable globals_ref Fresh the_table_name the_table_id = 
    do stale <- liftM global_stale $ readIORef globals_ref
       if stale then return Nothing else driverRequestTable globals_ref the_table_name the_table_id
driverGetTable globals_ref New the_table_name the_table_id = driverRequestTable globals_ref the_table_name the_table_id
driverGetTable globals_ref Old the_table_name the_table_id =
    do liftM ((find (\x -> table_name x == the_table_name && table_id x == the_table_id)) . restate_tables . global_old_engine_state) $ readIORef globals_ref
driverGetTable globals_ref Anything the_table_name the_table_id =
    do new <- driverGetTable globals_ref New the_table_name the_table_id
       old <- driverGetTable globals_ref Old the_table_name the_table_id
       return $ listToMaybe $ catMaybes [new,old]


-- |
-- Retrives a table from the engine, using a "game query ..." statement and
-- parsing any "begin-table ..." section that results.  You need to provide
-- both a table-name (the first string parameter) and a table id (the second).
-- In many cases a particular table-name always has a single table with an id
-- of "0".  In other cases there are many tables with the same name and different
-- ids.
--
driverRequestTable :: IORef RoguestarGlobals -> String -> String -> IO (Maybe RoguestarTable)
driverRequestTable globals_ref the_table_name the_table_id =
    do globals <- readIORef globals_ref
       let table = find (\x -> table_name x == the_table_name && table_id x == the_table_id) $ restate_tables $ global_engine_state globals 
	   in case table of
			 Nothing -> do driverWrite globals_ref ("game query " ++ the_table_name ++ "\n")
				       return Nothing
			 Just just_table -> return $ Just just_table

driverAction :: IORef RoguestarGlobals -> [String] -> IO ()
driverAction globals_ref strs = 
    do modifyIORef globals_ref (\globals -> globals { global_stale = True})
       driverWrite globals_ref ("game action " ++ (unwords strs) ++ "\n")

-- |
-- Writes the specified command to standard output, automatically triggering a read, in parallel.
-- driverWrite will never write the same string twice between calls to driverReset,
-- but it will ensure that a read attempt occurs no matter what.
--
driverWrite :: IORef RoguestarGlobals -> String -> IO ()
driverWrite globals_ref str = do already_sent <- liftM (elem str . global_engine_output_lines) $ readIORef globals_ref
				 unless already_sent $ 
				     do modifyIORef globals_ref $ (\globals -> globals { global_engine_output_lines=str:global_engine_output_lines globals })
					driverWrite_ globals_ref str
				 driverRead globals_ref --extra read in case write never happened (harmless)

driverWrite_ :: IORef RoguestarGlobals -> String -> IO ()
driverWrite_ globals_ref "" = do hFlush stdout
				 driverRead globals_ref
driverWrite_ globals_ref str = do driverRead globals_ref
				  putChar $ head str
				  --hPutChar stderr $ head str -- uncomment to see everything we write in stderr
				  driverWrite_ globals_ref $ tail str

-- |
-- Read one character if it is available.
--
maybeRead :: IO (Maybe Char)
maybeRead = do ready <- hReady stdin
	       case ready of
			  False -> return Nothing
			  True -> do char <- getChar
				     return $ Just char

-- |
-- Read and store data from the engine.
--
driverRead :: IORef RoguestarGlobals -> IO ()
driverRead globals_ref = 
    do maybe_next_char <- maybeRead
       case maybe_next_char of
			    Nothing -> return ()
 			    Just '\n' -> do modifyIORef globals_ref (\globals -> globals { global_engine_input_lines=(reverse $ global_engine_input_line_fragment globals) : global_engine_input_lines globals,
                                                                                           global_engine_input_line_fragment=""});
				            driverUpdate globals_ref
			    Just next_char -> modifyIORef globals_ref (\globals -> globals { global_engine_input_line_fragment=(next_char : global_engine_input_line_fragment globals) })
       when (isJust maybe_next_char) $ do --hPutChar stderr $ fromJust maybe_next_char -- uncomment to see everything we read in stderr
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
    do final_state <- foldM (interpretLine globals_ref) DINeutral =<< liftM (reverse . global_engine_input_lines) (readIORef globals_ref)
       when (final_state /= DINeutral) $ do printText globals_ref Untranslated "interpretText concluded in a non-neutral state, which was:"
					    printText globals_ref Untranslated (show final_state)
       modifyIORef globals_ref (\globals -> globals { global_engine_input_lines = [] })

modifyEngineState :: IORef RoguestarGlobals -> (RoguestarEngineState -> RoguestarEngineState) -> IO ()
modifyEngineState globals_ref fn = modifyIORef globals_ref (\globals -> globals { global_engine_state = fn $ global_engine_state globals }) 

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
    do modifyIORef globals_ref (\globals -> globals { global_engine_state = global_engine_state roguestar_globals_0,
                                                      global_old_engine_state = global_engine_state globals,
                                                      global_stale = False,
					              global_engine_output_lines = global_engine_output_lines roguestar_globals_0,
					              global_dones = global_dones globals + 1 })
       return DINeutral

interpretLine globals_ref distate "done" = 
    do printText globals_ref Untranslated ("gui-side protocol error: unexpected \"done\" in " ++ (show distate) ++ " state.")
       return DIError

interpretLine _ DINeutral "over" = return DINeutral

interpretLine globals_ref (DIScanningTable {}) "over" = 
    do printText globals_ref Untranslated "gui-side protocol error: 'over' issued while reading a data table"
       return DIError

interpretLine globals_ref DINeutral str | (head $ words str) == "answer:" && (length $ words str) == 3 =
    do modifyEngineState globals_ref (\engine_state -> engine_state { restate_answers = (words str !! 1,words str !! 2):restate_answers engine_state })
       return DINeutral

interpretLine _ DINeutral str | (head $ words str) == "begin-table" && (length $ words str) > 3 = 
    do time <- getTime
       let table_start_data = words str
       return $ DIScanningTable $ RoguestarTable {
						  table_created = time,
				                  table_name = table_start_data !! 1,
		                                  table_id = table_start_data !! 2,
		                                  table_header = drop 3 table_start_data,
                                                  table_data = []}

interpretLine globals_ref _ str | (head $ words str) == "begin-table" =
    do printText globals_ref Untranslated "gui-side protocol error: incomplete begin-table header"
       return DIError

interpretLine globals_ref (DIScanningTable table) str | (head $ words str) == "end-table" =
  do modifyEngineState globals_ref (\engine_state -> engine_state { restate_tables = (table { table_data=reverse $ table_data table}):restate_tables engine_state })
     return DINeutral

interpretLine globals_ref (DIScanningTable table) str = let table_row = words str
							    in (if length table_row == (length $ table_header table)
								then return $ DIScanningTable (table { table_data = table_row : table_data table })
								else do { printText globals_ref Untranslated "gui-side protocol error: malformed table row";
									  return DIError })

interpretLine globals_ref _ str = do printText globals_ref Untranslated (str)
				     return DINeutral