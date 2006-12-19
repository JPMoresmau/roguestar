--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

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