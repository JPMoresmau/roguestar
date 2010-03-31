module Strings
    (replace,capitalize,hrstring)
    where

import Data.Char
import Data.List

-- | Replace all instances of the first string with the second string in the third string.
-- @replace "old" "new" "What's old?"@
replace :: String -> String -> String -> String
replace _ _ [] = []
replace a b s = case stripPrefix a s of
    Nothing -> head s : replace a b (tail s)
    Just s' -> b ++ replace a b s'

-- | Just capitalize the first letter of the string.
capitalize :: String -> String
capitalize [] = []
capitalize (s:ss) = toUpper s : ss

-- | Human readable strings, when we can't just rip the plaintext from the protocol.
hrstring :: String -> String
hrstring "str" =   "Strength     "
hrstring "spd" =   "Speed        "
hrstring "con" =   "Endurance    "
hrstring "int" =   "Intellect    "
hrstring "per" =   "Perception   "
hrstring "cha" =   "Charisma     "
hrstring "mind" =  "Mindfulness  "
hrstring "maxhp" = "Health       "
hrstring "forceadept" = "force adept"
hrstring x = map (\c -> if c == '_' then ' ' else c) x


