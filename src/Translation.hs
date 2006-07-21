module Translation
    (Language(..),
     translator,
     tr)
    where

data Language = English
	      deriving (Eq,Enum,Show)

-- |
-- If the string corresponds to a language for which a translation exists,
-- answers the language, otherwise Nothing.
--
translator :: String -> Maybe Language
translator "en" = Just English
translator _ = Nothing

tr :: Language -> [String] -> String

tr English ["window-title"] = "RogueStar - OpenGL"
tr _ args = unwords args