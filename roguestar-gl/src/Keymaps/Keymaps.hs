-- | 'Keymap's are simple mappings from a sequence of keystrokes to the
-- names of various commands.
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
import qualified KeyStroke as K
import Data.Monoid

type Keymap = [(K.KeyString,B.ByteString)]
type KeymapName = String

-- 'fixKeymap' processes a keymap to make it usable, by:
-- * removing erroneous whitespace
-- * placing \\n and \\r at the end of multi-key keystrokes that should be
--   activated via the enter key.
fixKeymap :: Keymap -> Keymap
fixKeymap = concatMap $ \(keystrokes,action_name) ->
    case () of
        () | K.null keystrokes -> []
        () | K.length keystrokes == 1 -> [(keystrokes,action_name)]
        () -> [(keystrokes `mappend` "&KeyActivate;",action_name)]

-- | 'validKeyMap' reduces a 'Keymap' to one that contains only those actions
-- that are valid at the instant of the atomic transaction.
validKeyMap :: ActionInput -> Keymap -> STM Keymap
validKeyMap action_input raw_keymap =
    do valid_actions <- getValidActions action_input Nothing
       return $ filter (\x -> snd x `elem` valid_actions) raw_keymap

-- | 'filterKeySequence' transforms a key sequence, providing services such as:
-- * blanking the input buffer if there is no possible completion and
-- * performing tab complation
filterKeySequence :: ActionInput -> Keymap -> K.KeyString -> STM K.KeyString
filterKeySequence _ _ key_sequence |
    (length $ words $ K.asString key_sequence) /= 1 = return ""
filterKeySequence action_input keymap key_sequence =
    do valid_key_map <- validKeyMap action_input keymap
       let is_tab_key_completion = "&KeyTab;" `K.isSuffixOf` key_sequence &&
                                   key_sequence /= "&KeyTab;"
       let stripped_key_sequence = if is_tab_key_completion
               then K.init key_sequence
               else key_sequence
       let possible_completions =
               filter (\x -> elem (stripped_key_sequence) $ K.inits x) $
                   map fst $ valid_key_map
       return $ case length possible_completions of
           -- if there are no possible completions, clear the buffer
           0 -> ""
           -- if the trailing input isn't a tab, chill
           _ | not is_tab_key_completion -> key_sequence
           -- if there's only one completion, get it, but strip
           -- the trailing return in case they didn't want
           -- to exec that completion.
           1 -> if key_sequence /= head possible_completions
                    then K.init $ head possible_completions
                    else stripped_key_sequence
           -- get the best completion we can find
           _ -> maximumBy (\x y -> compare (K.length x) (K.length y)) $
                    foldr1 intersect $ map K.inits possible_completions

-- | 'keysToActionNames' gets a list of the names of all actions that could be
-- executed by the given key sequence at this instant.  A null result
-- indicates that the key sequence isn't a valid command.  A result with more
-- than one element indicates a collision in the key map, which is an error.

keysToActionNames :: ActionInput -> Keymap -> K.KeyString -> STM [B.ByteString]
keysToActionNames action_input keymap key_sequence =
    liftM (map snd . filter (\x -> fst x == key_sequence)) $ validKeyMap action_input keymap 

-- 'actionNameToKeys' provides reverse lookup versus keysToActionNames.
actionNameToKeys :: ActionInput -> Keymap -> B.ByteString -> STM [K.KeyString]
actionNameToKeys action_input keymap action_name =
    liftM (map fst . filter (\x -> snd x == action_name)) $ validKeyMap action_input keymap

