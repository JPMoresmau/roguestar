module PrintTables
    (formatTable)
    where

import Tables
import Translation
import Data.Maybe

-- |
-- Helper function for formatTable.  If the value string is Nothing, returns an empty string.
-- If the data string is Just something, translates the property string and concatenates the
-- raw data string.  Format is (property,Maybe value).
--
formatLine :: Language -> RoguestarTable -> (String,Maybe String) -> String
formatLine _ _ (_,Nothing) = ""
formatLine language table (str1,Just str2) = translateStr language ["table",table_name table,str1] ++ str2 ++ "\n"

-- |
-- As formatLine, but translates both the property string and the value string.
--
formatLine2 :: Language -> RoguestarTable -> (String,Maybe String) -> String
formatLine2 language table (str1,Just str2) = translateStr language ["table",table_name table,str1] ++
					      translateStr language ["table-data",table_name table,str1,str2] ++
					      "\n"
formatLine2 _ _ (_,Nothing) = ""

-- |
-- AS formatLine, but translates both strings.

-- |
-- Transforms a table into a human-readable display.
--
-- A goal of this function is to never assume that any particular piece of data in a table is there,
-- even if the table is there.  This facilitates experimentation with the engine.
--
formatTable :: Language -> RoguestarTable -> Maybe String

formatTable language table | table_name table == "player-stats" =
			       let the_table_data = tableSelect2 table ("property","value")
				   species_name = lookup "species" the_table_data
				   creature_random_id = lookup "random-id" the_table_data
				   name_str = (if and [isJust species_name,isJust creature_random_id]
					       then translateStr language ["a-creature-named",fromJust species_name,fromJust creature_random_id] ++ "\n"
					       else "")
				   (hp,maxHP) = (lookup "hp" the_table_data,lookup "maxhp" the_table_data)
				   hp_str = case (hp,maxHP) of
							    (Nothing,Nothing) -> []
							    (Just x,Nothing) -> formatLine language table ("hp", Just x)
							    (Nothing,Just y) -> formatLine language table ("maxhp", Just y)
							    (Just x,Just y) -> formatLine language table ("hp/maxhp", Just $ x ++ "/" ++ y)
				   gender_str = formatLine2 language table ("gender",lookup "gender" the_table_data)
				   in Just $ (name_str ++ 
					      gender_str ++
					      (concatMap (\x -> formatLine2 language table (x, lookup x the_table_data))
					       ["str","dex","con","int","per","cha","mind"]) ++
					      hp_str)

formatTable _ _ = Nothing