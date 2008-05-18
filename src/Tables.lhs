\section{Tables}

\begin{code}
module Tables
    (RoguestarTable(..),
     TableDataFormat(..),
     tableSelect,
     tableSelectFormatted,
     tableLookup,
     tableLookupInteger)
    where

import Data.List
import Data.Maybe
import RSAGL.Time
import Control.Monad
\end{code}

\texttt{RoguestarTable} is a crude implementation of a relational data table that is used to represent information that has been sent to us from roguestar-engine.

\begin{code}
data RoguestarTable = RoguestarTable { table_created :: Time, table_name, table_id :: String, table_header :: [String], table_data :: [[String]] }
		      deriving (Eq,Show)
\end{code}

\texttt{tableSelect} selects rows from a table, like the SQL select statement.
For example: \texttt{tableSelect people ["name","sex","phone-number"] = [["bob","male","123-4567"],["susan","female","987-6543"]]}
If a given header is not in the table, it lists \texttt{"???"} as the value.

There is a guarantee that all select functions will return results in the same order.

\begin{code}
tableSelect :: RoguestarTable -> [String] -> [[String]]
tableSelect table headers = let header_indices = map (\x -> elemIndex x $ table_header table) headers
				in map (rowSelect header_indices) $ table_data table

rowSelect :: [Maybe Int] -> [String] -> [String]
rowSelect (Nothing:more) row = "???" : rowSelect more row
rowSelect (Just x:more) row = (row !! x) : rowSelect more row
rowSelect [] _ = []
\end{code}

\texttt{tableSelectFormatted} allows Selection of arbitrary strings and integers.

\begin{code}
tableSelectFormatted :: RoguestarTable -> [TableDataFormat String String] -> [[TableDataFormat String Integer]]
tableSelectFormatted table headers = map (toFormat headers) $ tableSelect table (map toString headers)
\end{code}

\texttt{tableLookup table ("name","phone-number") "bob"} yields Bob's phone number, or nothing if "bob" isn't in the table.

\begin{code}
tableLookup :: RoguestarTable -> (String,String) -> String -> Maybe String
tableLookup table (k,v) x = fmap (!! 1) $ find ((== x) . head) $ tableSelect table [k,v]
\end{code}

\texttt{tableLookupInteger} works as \texttt{tableLookup}, but answers an \texttt{Integer}.

\begin{code}
tableLookupInteger :: RoguestarTable -> (String,String) -> String -> Maybe Integer
tableLookupInteger table headers x = tableLookup table headers x >>= readInteger
\end{code}

\subsection{Formatting Arbitrary Table Elements}

\begin{code}
data TableDataFormat s n = TDString s
                         | TDNumber n

readInteger :: String -> Maybe Integer
readInteger = listToMaybe . map fst . reads

toString :: TableDataFormat String String -> String
toString (TDString str) = str
toString (TDNumber str) = str

toFormat :: [TableDataFormat a b] -> [String] -> [TableDataFormat String Integer]
toFormat headers row = zipWith toFormat_ headers row
    where toFormat_ (TDString {}) str = TDString str
          toFormat_ (TDNumber {}) str = maybe (TDString str) TDNumber $ readInteger str
\end{code}
