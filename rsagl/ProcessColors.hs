#!/usr/bin/runghc

import RSAGL.Modeling.Material
import Control.Monad
import Data.List
import Numeric

data ColorTableEntry = ColorTableEntry { cte_name :: String, cte_red, cte_green, cte_blue :: Int }

readColorTableEntry :: String -> ColorTableEntry
readColorTableEntry s = ColorTableEntry name (read r :: Int) (read g :: Int) (read b :: Int)
    where [name,r,g,b] = words s

cteToColor :: ColorTableEntry -> RGB
cteToColor cte = rgb256 (cte_red cte) (cte_green cte) (cte_blue cte)

cteToHaskell :: ColorTableEntry -> String
cteToHaskell (ColorTableEntry s r g b) = s ++ " :: RGB\n" ++ s ++ " = rgb256 " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ "\n"

wrapHaskellModule :: [ColorTableEntry] -> String -> String
wrapHaskellModule cte s = ("module RSAGL.Modeling.RSAGLColors (" ++ (listColors cte) ++ ") where") ++ 
                          "\n\nimport Prelude ()" ++
                          "\nimport RSAGL.Modeling.Material\n\n" ++ s

listColors :: [ColorTableEntry] -> String
listColors = concat . intersperse "," . map cte_name

wrapHTMLFile :: String -> String
wrapHTMLFile s = "<html><head><title>RSAGL Color Tables</title></head><body>" ++ s ++ "</body></html>"

cteToHTML :: ColorTableEntry -> String
cteToHTML (ColorTableEntry s r g b) = "<p><div style=\"background-color:" ++ (zeroes $ showHex (r * 0x10000 + g * 0x100 + b) "") ++ "\">" 
                                      ++ s ++ " " ++ show r ++ " " ++ show g ++ " " ++ show b ++ "</div>"
    where zeroes x = replicate (6 - length x) '0' ++ x

cteSubtable :: String -> [ColorTableEntry] -> String
cteSubtable name color_tables = "<h4>" ++ name ++ "</h4>" ++ (unlines . map cteToHTML) color_tables

colorTablesToHTML :: [ColorTableEntry] -> String
colorTablesToHTML color_tables =
   cteSubtable "All Colors" color_tables ++
   cteSubtable "By Luminance" (reverse $ sortBy luminance color_tables) ++
   cteSubtable "By Red" (takeHalf $ sortBy byred color_tables) ++
   cteSubtable "By Green" (takeHalf $ sortBy bygreen color_tables) ++
   cteSubtable "By Blue" (takeHalf $ sortBy byblue color_tables)
       where luminance x y = compare (cteBrightness x) (cteBrightness y)
             byred x y = compare (toRed y ^ 2 / flatBrightness y) (toRed x ^ 2 / flatBrightness x)
             bygreen x y = compare (toGreen y ^ 2 / flatBrightness y) (toGreen x ^ 2 / flatBrightness x)
             byblue x y = compare (toBlue y ^ 2 / flatBrightness y) (toBlue x ^ 2 / flatBrightness x)
             cteBrightness = realToFrac . (1 +) . brightness . cteToColor
             flatBrightness x = toRed x * toGreen x * toBlue x
             toRed = realToFrac . cte_red
             toGreen = realToFrac . cte_green
             toBlue = realToFrac . cte_blue
             takeHalf x = take (length x `div` 2) x

isComment :: String -> Bool
isComment s | null (words s) = True
isComment s = ["#"] == (map (take 1) $ take 1 $ words s)

main :: IO ()
main = do color_tables <- liftM (map readColorTableEntry . filter (not . isComment) . lines) $ readFile "rsagl-rgb.txt"
          writeFile "RSAGL/Modeling/RSAGLColors.hs" $ wrapHaskellModule color_tables $ (unlines . map cteToHaskell) color_tables
          writeFile "rsagl-rgb.html" $ wrapHTMLFile $ colorTablesToHTML color_tables
