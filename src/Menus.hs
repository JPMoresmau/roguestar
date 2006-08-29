module Menus
    (printMenu)
    where

import PrintText
import Data.IORef
import Globals
import Keymaps
import Data.List
import Translation
import Control.Monad

printMenu :: IORef RoguestarGlobals -> String -> [String] -> IO ()
printMenu globals_ref menu_title menu_items = 
    do printTranslated globals_ref UserQuery $ ["menu-title",menu_title]
       mapM_ (printMenuItem globals_ref menu_title) menu_items

printMenuItem :: IORef RoguestarGlobals -> String -> String -> IO ()
printMenuItem globals_ref menu_title menu_item =
    do globals <- readIORef globals_ref
       keys <- actionNameToKeys globals_ref menu_item
       when (length keys > 0) $ do shortest_key <- return $ unwords $ words $ minimumBy (\x -> \y -> compare (length x) (length y)) keys
				   printText globals_ref Information (shortest_key ++ " - " ++ translateStr (global_language globals) ["menu-item",menu_title,menu_item])