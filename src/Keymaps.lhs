\section{Keymaps}

\texttt{Keymap}s are simple mappings from a sequence of keystrokes to the names of various commands.

\begin{code}
module Keymaps
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

type Keymap = [(String,String)]
type KeymapName = String
\end{code}

\texttt{fixKeymap} processes a keymap to make it usable.  If you type in a multi-character command and it executes before you press enter,
you forgot to use \texttt{fixKeymap}.

\begin{code}
fixKeymap :: Keymap -> Keymap
fixKeymap = concatMap $ \(x,y) -> 
    case x of
        [c] -> [([c],y)]
	('>':keystrokes) | (not $ null keystrokes) -> [(keystrokes,y)]
	command -> let keystrokes = concat $ intersperse "-" $ words command in [(keystrokes ++ "\r",y),(keystrokes ++ "\n",y)]
\end{code}

\texttt{validKeyMap} reduces a \texttt{Keymap} to one that contains only those actions that are valid at this instant.

\begin{code}
validKeyMap :: ActionInput -> Keymap -> IO [(String,String)]
validKeyMap action_input raw_keymap =
    do valid_actions <- getValidActions action_input Nothing
       return $ filter (\x -> snd x `elem` valid_actions) raw_keymap
\end{code}

\texttt{filterKeySequence} transforms a key sequence into either the empty string, if it isn't the prefix of any valid action,
or completes the key sequence if it is the prefix of exactly one valid action.

\begin{code}
filterKeySequence :: ActionInput -> Keymap -> String -> IO String
filterKeySequence _ _ key_sequence | (length $ words key_sequence) == 0 = return ""
filterKeySequence action_input keymap key_sequence =
    do valid_key_map <- validKeyMap action_input keymap
       let possible_completions = filter (\x -> elem key_sequence $ inits x) $ map fst $ valid_key_map
       return $ case length possible_completions of
		 0 -> ""
		 1 -> if key_sequence /= head possible_completions
		      then init $ head possible_completions
		      else key_sequence
		 _ -> maximumBy (\x -> \y -> compare (length x) (length y)) $ foldr1 intersect $ map inits possible_completions
\end{code}

\texttt{keysToActionNames} gets a list of the names of all action that could be exected by the given key sequence at this instant.
A result of zero indicates that the key sequence isn't a valid command.  A result with more than one element indicates
a collision in the key map, which is an error.

\begin{code}
keysToActionNames :: ActionInput -> Keymap -> String -> IO [String]
keysToActionNames action_input keymap key_sequence = 
    liftM (map snd . filter (\x -> fst x == key_sequence)) $ validKeyMap action_input keymap 
\end{code}

\texttt{actionNameToKeys} provides reverse lookup versus keysToActionNames.

\begin{code}
actionNameToKeys :: ActionInput -> Keymap -> String -> IO [String]
actionNameToKeys action_input keymap action_name = 
    liftM (map fst . filter (\x -> snd x == action_name)) $  validKeyMap action_input keymap
\end{code}
