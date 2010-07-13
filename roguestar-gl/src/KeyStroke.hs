module KeyStroke
    (KeyStroke(..),
     KeyString,
     asString)
    where

data KeyStroke =
    Stroke Char
  | KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
    deriving (Eq,Read,Show)

type KeyString = [KeyStroke]

asString :: KeyString -> String
asString [] = []
asString (Stroke c: rest) = c:asString rest
asString (s:rest) = ":::" ++ show s ++ asString rest

