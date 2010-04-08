{-# LANGUAGE OverloadedStrings #-}
module Strings
    (replace,capitalize,hrstring)
    where

import Data.Char
import qualified Data.ByteString.Char8 as B

-- | Replace all instances of the first string with the second string in the third string.
-- @replace "old" "new" "What's old?"@
replace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replace a b s = case B.breakSubstring a s of
    (x,y) | B.null y  -> x
    (x,y) | otherwise -> x `B.append` b `B.append` replace a b (B.drop (B.length a) y)

-- | Just capitalize the first letter of the string.
capitalize :: B.ByteString -> B.ByteString
capitalize s = case B.uncons s of
    Nothing -> s
    Just (c,s') -> toUpper c `B.cons` s'

-- | Human readable strings, when we can't just rip the plaintext from the protocol.
hrstring :: B.ByteString -> B.ByteString
hrstring "str" =   "Strength     "
hrstring "spd" =   "Speed        "
hrstring "con" =   "Endurance    "
hrstring "int" =   "Intellect    "
hrstring "per" =   "Perception   "
hrstring "cha" =   "Charisma     "
hrstring "mind" =  "Mindfulness  "
hrstring "maxhp" = "Health       "
hrstring "forceadept" = "force adept"
hrstring x = B.map (\c -> if c == '_' then ' ' else c) x


