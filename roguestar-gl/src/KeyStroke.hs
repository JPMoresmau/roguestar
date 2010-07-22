{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module KeyStroke
    (KeyStroke(..),
     KeyString,
     asString,
     prettyString,
     keyStroke,
     KeyStroke.null,
     KeyStroke.init,
     KeyStroke.length,
     KeyStroke.isSuffixOf,
     KeyStroke.inits)
    where

import qualified Data.ByteString.Char8 as B
import Data.String
import Data.Char
import Data.Monoid
import qualified Data.List as List

data KeyStroke =
    Stroke Char
  | KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyActivate -- spacebar, and any enter key
  | KeyEscape   -- escape, delete, backspace, etc
  | KeyTab      -- tab and any key that might be used for completions
  | KeyAmpersand
  | KeySemicolon
  | NumPad1
  | NumPad2
  | NumPad3
  | NumPad4
  | NumPad5
  | NumPad6
  | NumPad7
  | NumPad8
  | NumPad9
  | NumPad0
  | KeyIgnored
    deriving (Eq,Read,Show)

newtype KeyString = KeyString { fromKeyString :: [KeyStroke] }
    deriving (Eq,Read,Show,Monoid)

null :: KeyString -> Bool
null = Prelude.null . fromKeyString

init :: KeyString -> KeyString
init = KeyString . Prelude.init . fromKeyString

inits :: KeyString -> [KeyString]
inits = map KeyString . List.inits . fromKeyString

length :: KeyString -> Int
length = Prelude.length . fromKeyString

isSuffixOf :: KeyString -> KeyString -> Bool
isSuffixOf a b = fromKeyString a `List.isSuffixOf` fromKeyString b

keyStroke :: KeyStroke -> KeyString
keyStroke s = KeyString [s]

prettyString :: KeyString -> String
prettyString (KeyString ks) = concatMap f ks
    where f (Stroke c) = [c]
          f KeyUp        = "[UP]"
          f KeyDown      = "[DOWN]"
          f KeyLeft      = "[LEFT]"
          f KeyRight     = "[RIGHT]"
          f KeyActivate  = "[ENTER]"
          f KeyEscape    = "[ESC]"
          f KeyTab       = "^T"
          f KeyAmpersand = "&"
          f KeySemicolon = ";"
          f NumPad1      = "[1]"
          f NumPad2      = "[2]"
          f NumPad3      = "[3]"
          f NumPad4      = "[4]"
          f NumPad5      = "[5]"
          f NumPad6      = "[6]"
          f NumPad7      = "[7]"
          f NumPad8      = "[8]"
          f NumPad9      = "[9]"
          f KeyIgnored   = "[IGNORED]"

asString :: (IsString s) => KeyString -> s
asString = fromString . asString_
    where asString_ (KeyString []) = []
          asString_ (KeyString (Stroke c: rest)) = c:asString (KeyString rest)
          asString_ (KeyString (KeyIgnored:rest)) = asString_ (KeyString rest)
          asString_ (KeyString (s:rest)) = "&" ++ show s ++ ";" ++ asString (KeyString rest)

instance IsString KeyString where
    fromString s = KeyString $ fromString_ s

fromString_ :: String -> [KeyStroke]
fromString_ [] = []
fromString_ ('\n':rest) = KeyActivate:fromString_ rest
fromString_ ('\r':rest) = KeyActivate:fromString_ rest
fromString_ ('\t':rest) = KeyTab:fromString_ rest
fromString_ ('\ESC':rest) = KeyEscape:fromString_ rest
fromString_ ('&':rest) | ';' `elem` rest =
    key_string : fromString_ (drop 1 $ dropWhile (/= ';') rest)
    where key_string = case map toUpper (takeWhile (/= ';') rest) of
              "KEYUP" -> KeyUp
              "KEYDOWN" -> KeyDown
              "KEYLEFT" -> KeyLeft
              "KEYRIGHT" -> KeyRight
              "KEYACTIVATE" -> KeyActivate
              "KEYESCAPE" -> KeyEscape
              "KEYTAB" -> KeyTab
              "NUMPAD1" -> NumPad1
              "NUMPAD2" -> NumPad2
              "NUMPAD3" -> NumPad3
              "NUMPAD4" -> NumPad4
              "NUMPAD5" -> NumPad5
              "NUMPAD6" -> NumPad6
              "NUMPAD7" -> NumPad7
              "NUMPAD8" -> NumPad8
              "NUMPAD9" -> NumPad9
              "NUMPAD0" -> NumPad0
              _ -> KeyIgnored
fromString_ ('&':rest) = KeyAmpersand:fromString_ rest
fromString_ (';':rest) = KeySemicolon:fromString_ rest
fromString_ (c:rest) = Stroke c:fromString_ rest

