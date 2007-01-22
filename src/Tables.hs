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

module Tables
    (RoguestarTable(..),
     TableDataFormat(..),
     tableSelect,
     tableSelect1,
     tableSelect2,
     tableSelect2Integer,
     tableSelectFormatted,
     tableLookup,
     tableLookupInteger,
     tableAge)
    where

import Data.List
import Data.Maybe
import Seconds
import Control.Monad

-- |
-- This is a simple implementation of a relational data table, and is always used to represent information
-- that has been sent to us from roguestar-engine.
--
data RoguestarTable = RoguestarTable { table_created :: Seconds, table_name, table_id :: String, table_header :: [String], table_data :: [[String]] }
		      deriving (Eq,Show)

data TableDataFormat s n = TDString s
                         | TDNumber n

-- |
-- The age of this table.
--
tableAge :: (Fractional n) => RoguestarTable -> IO n
tableAge = secondsSince . table_created

-- |
-- Select from a table, like the SQL select statement.
-- For example:
-- tableSelect people ["name","sex","phone-number"] = [["bob","male","123-4567"],["susan","female","987-6543"]]
-- If a given header is not in the table, lists "???" as the value.
--
-- There is a guarantee that all select functions will return results in the same order.
--
tableSelect :: RoguestarTable -> [String] -> [[String]]
tableSelect table headers = let header_indices = map (\x -> elemIndex x $ table_header table) headers
				in map (rowSelect header_indices) $ table_data table

rowSelect :: [Maybe Int] -> [String] -> [String]
rowSelect (Nothing:more) row = "???" : rowSelect more row
rowSelect (Just x:more) row = (row !! x) : rowSelect more row
rowSelect [] _ = []

-- |
-- As tableSelect, but select a single header.
--
tableSelect1 :: RoguestarTable -> String -> [String]
tableSelect1 table header = map head $ tableSelect table [header]

-- |
-- As tableSelect, but select exactly two headers as a pair.
--
tableSelect2 :: RoguestarTable -> (String,String) -> [(String,String)]
tableSelect2 table headers = map (\x -> (x !! 0,x !! 1)) $ tableSelect table [fst headers,snd headers]

-- |
-- As tableSelect2, converting the second element into a Maybe Integer.
--
tableSelect2Integer :: RoguestarTable -> (String,String) -> [(String,Maybe Integer)]
tableSelect2Integer table headers = map (\x -> (fst x,readInteger $ snd x)) $ tableSelect2 table headers

-- |
-- Select arbitrary strings and integers.
--
tableSelectFormatted :: RoguestarTable -> [TableDataFormat String String] -> [[TableDataFormat String Integer]]
tableSelectFormatted table headers = map (toFormat headers) $ tableSelect table (map toString headers)

-- |
-- tableLookup table ("name","phone-number") "bob" = bob's phone number, or nothing if "bob" isn't in the table.
--
tableLookup :: RoguestarTable -> (String,String) -> String -> Maybe String
tableLookup table headers value = lookup value $ tableSelect2 table headers

-- |
-- As tableLookup, but answers an integer.
--
tableLookupInteger :: RoguestarTable -> (String,String) -> String -> Maybe Integer
tableLookupInteger table headers value = fromMaybe Nothing $ lookup value $ tableSelect2Integer table headers

readInteger :: String -> Maybe Integer
readInteger = listToMaybe . map fst . reads

toString :: TableDataFormat String String -> String
toString (TDString str) = str
toString (TDNumber str) = str

toFormat :: [TableDataFormat a b] -> [String] -> [TableDataFormat String Integer]
toFormat headers row = zipWith toFormat_ headers row
    where toFormat_ (TDString {}) str = TDString str
          toFormat_ (TDNumber {}) str = maybe (TDString str) TDNumber $ readInteger str