module String
    (strToLower,
     strToUpper,
     capitalize,
     titleCapitalize,
     padLeft,
     padRight)
    where

import Data.Char

strToLower :: String -> String
strToLower = map toLower

strToUpper :: String -> String
strToUpper = map toUpper

capitalize :: String -> String
capitalize "" = ""
capitalize str = let str' = unwords $ words str
		     in (toUpper $ head str') : (tail str')

titleCapitalize :: String -> String
titleCapitalize str = capitalize $ unwords $ map capitalizeTitleWord $ words str
    where capitalizeTitleWord "the" = "the"
	  capitalizeTitleWord "a" = "a"
	  capitalizeTitleWord "an" = "an"
	  capitalizeTitleWord x = capitalize x

padLeft :: Int -> String -> String
padLeft n str = replicate (max 1 $ n - length str) ' ' ++ str

padRight :: Int -> String -> String
padRight n str = str ++ replicate (max 1 $ n - length str) ' '