{-# LANGUAGE OverloadedStrings #-}
module Tables
    (RoguestarTable(..),
     TableDataFormat(..),
     tableSelect,
     tableSelectFormatted,
     tableLookup,
     tableLookupInteger,
     readInteger)
    where

import Data.List
import RSAGL.FRP.Time
import qualified Data.ByteString.Char8 as B

-- | 'RoguestarTable' is a crude implementation of a relational data table that is used to represent information that has been sent to us from roguestar-engine.
data RoguestarTable = RoguestarTable { table_created :: Time, table_name, table_id :: B.ByteString, table_header :: [B.ByteString], table_data :: [[B.ByteString]] }
		      deriving (Eq,Show)

-- | 'tableSelect' selects rows from a table, like the SQL select statement.
-- For example: \texttt{tableSelect people ["name","sex","phone-number"] = [["bob","male","123-4567"],["susan","female","987-6543"]]}
-- If a given header is not in the table, it lists \texttt{"???"} as the value.
--
-- There is a guarantee that all select functions will return results in the same order.
tableSelect :: RoguestarTable -> [B.ByteString] -> [[B.ByteString]]
tableSelect table headers = let header_indices = map (\x -> elemIndex x $ table_header table) headers
				in map (rowSelect header_indices) $ table_data table

rowSelect :: [Maybe Int] -> [B.ByteString] -> [B.ByteString]
rowSelect (Nothing:more) row = "???" : rowSelect more row
rowSelect (Just x:more) row = (row !! x) : rowSelect more row
rowSelect [] _ = []

-- | 'tableSelectFormatted' allows Selection of arbitrary strings and integers.
tableSelectFormatted :: RoguestarTable -> [TableDataFormat B.ByteString B.ByteString] -> [[TableDataFormat B.ByteString Integer]]
tableSelectFormatted table headers = map (toFormat headers) $ tableSelect table (map toString headers)

-- | "tableLookup table \(\"name\",\"phone-number\"\) \"bob\"" yields Bob\'s phone number, or nothing if \"bob\" isn\'t in the table.
tableLookup :: RoguestarTable -> (B.ByteString,B.ByteString) -> B.ByteString -> Maybe B.ByteString
tableLookup table (k,v) x = fmap (!! 1) $ find ((== x) . head) $ tableSelect table [k,v]

-- | 'tableLookupInteger' works as 'tableLookup', but answers an 'Integer'
tableLookupInteger :: RoguestarTable -> (B.ByteString,B.ByteString) -> B.ByteString -> Maybe Integer
tableLookupInteger table headers x = tableLookup table headers x >>= readInteger

data TableDataFormat s n = TDString s
                         | TDNumber n

readInteger :: B.ByteString -> Maybe Integer
readInteger = fmap fst . B.readInteger

toString :: TableDataFormat B.ByteString B.ByteString -> B.ByteString
toString (TDString str) = str
toString (TDNumber str) = str

toFormat :: [TableDataFormat a b] -> [B.ByteString] -> [TableDataFormat B.ByteString Integer]
toFormat headers row = zipWith toFormat_ headers row
    where toFormat_ (TDString {}) str = TDString str
          toFormat_ (TDNumber {}) str = maybe (TDString str) TDNumber $ readInteger str
