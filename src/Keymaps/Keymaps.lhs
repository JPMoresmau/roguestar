\section{Keymaps}

\texttt{Keymap}s are simple mappings from a sequence of keystrokes to the names of various commands.

\begin{code}

{-# LANGUAGE OverloadedStrings #-}

module Keymaps.Keymaps
    (Keymap,
     KeymapName,
     fixKeymap,
     filterKeySequence,
     keysToActionNames,
     actionNameToKeys)
    where

import Actions
import Data.List
import Control.Monad
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B

type Keymap = [(B.ByteString,B.ByteString)]
type KeymapName = String
\end{code}

\texttt{fixKeymap} processes a keymap to make it usable.

\begin{code}
fixKeymap :: Keymap -> Keymap
fixKeymap = concatMap $ \(keystrokes,action_name) -> 
    case () of
        () | B.length keystrokes == 1 -> [(keystrokes,action_name)]
	() | B.head keystrokes == '>' && (not $ B.null keystrokes) -> [(B.drop 1 keystrokes,action_name)]
	() -> let fixed_keystrokes = B.concat $ intersperse "-" $ B.words keystrokes 
                  in [(fixed_keystrokes `B.append` "\r",action_name),(keystrokes `B.append` "\n",action_name)]
\end{code}

\texttt{validKeyMap} reduces a \texttt{Keymap} to one that contains only those actions that are valid at this instant.

\begin{code}
validKeyMap :: ActionInput -> Keymap -> STM [(B.ByteString,B.ByteString)]
validKeyMap action_input raw_keymap =
    do valid_actions <- getValidActions action_input Nothing
       return $ filter (\x -> snd x `elem` valid_actions) raw_keymap
\end{code}

\texttt{filterKeySequence} transforms a key sequence into either the empty string, if it isn't the prefix of any valid action,
or completes the key sequence if it is the prefix of exactly one valid action.

\begin{code}
filterKeySequence :: ActionInput -> Keymap -> B.ByteString -> STM B.ByteString
filterKeySequence _ _ key_sequence | (length $ B.words key_sequence) == 0 = return ""
filterKeySequence action_input keymap key_sequence =
    do valid_key_map <- validKeyMap action_input keymap
       let possible_completions = filter (\x -> elem key_sequence $ B.inits x) $ map fst $ valid_key_map
       return $ case length possible_completions of
                 0 -> ""
                 1 -> if key_sequence /= head possible_completions
                      then B.init $ head possible_completions
                      else key_sequence
                 _ -> maximumBy (\x y -> compare (B.length x) (B.length y)) $ foldr1 intersect $ map B.inits possible_completions
\end{code}

\texttt{keysToActionNames} gets a list of the names of all action that could be exected by the given key sequence at this instant.
A result of zero indicates that the key sequence isn't a valid command.  A result with more than one element indicates
a collision in the key map, which is an error.

\begin{code}
keysToActionNames :: ActionInput -> Keymap -> B.ByteString -> STM [B.ByteString]
keysToActionNames action_input keymap key_sequence =
    liftM (map snd . filter (\x -> fst x == key_sequence)) $ validKeyMap action_input keymap 
\end{code}

\texttt{actionNameToKeys} provides reverse lookup versus keysToActionNames.

\begin{code}
actionNameToKeys :: ActionInput -> Keymap -> B.ByteString -> STM [B.ByteString]
actionNameToKeys action_input keymap action_name =
    liftM (map fst . filter (\x -> snd x == action_name)) $ validKeyMap action_input keymap
\end{code}
