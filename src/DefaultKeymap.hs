module DefaultKeymap
    (findKeymapOrDefault, builtin_keymaps, default_keymap, vim_keymap, numpad_keymap)
    where

import Data.Char
import Data.Maybe

import Keymaps

findKeymapOrDefault :: Maybe KeymapName -> Keymap
findKeymapOrDefault m_keymap_name = maybe default_keymap id (m_keymap_name >>= (flip lookup builtin_keymaps . (map toLower)))

builtin_keymaps :: [(KeymapName, Keymap)]
builtin_keymaps = [
 ("vim", vim_keymap),
 ("numpad", numpad_keymap)]

default_keymap :: Keymap
default_keymap = vim_keymap

vim_keymap :: Keymap
vim_keymap = keymapWithMovementKeys [
 ("k","move-n"),
 ("j","move-s"),
 ("h","move-w"),
 ("l","move-e"),
 ("y","move-nw"),
 ("u","move-ne"),
 ("b","move-sw"),
 ("n","move-se"),
 ("K","turn-n"),
 ("J","turn-s"),
 ("H","turn-w"),
 ("L","turn-e"),
 ("Y","turn-nw"),
 ("U","turn-ne"),
 ("B","turn-sw"),
 ("N","turn-se")]

numpad_keymap :: Keymap
numpad_keymap = keymapWithMovementKeys [
 ("8","move-n"),
 ("2","move-s"),
 ("4","move-w"),
 ("6","move-e"),
 ("7","move-nw"),
 ("9","move-ne"),
 ("1","move-sw"),
 ("3","move-se"),
 ("K","turn-n"),
 ("J","turn-s"),
 ("H","turn-w"),
 ("L","turn-e"),
 ("Y","turn-nw"),
 ("U","turn-ne"),
 ("B","turn-sw"),
 ("N","turn-se")]

keymapWithMovementKeys :: Keymap -> Keymap
keymapWithMovementKeys movement_keymap = fixKeymap $ movement_keymap ++ [
 (":move-n","move-n"),
 (":move-s","move-s"),
 (":move-w","move-w"),
 (":move-e","move-e"),
 (":move-nw","move-nw"),
 (":move-ne","move-ne"),
 (":move-sw","move-sw"),
 (":move-se","move-se"),
 (":turn-n","turn-n"),
 (":turn-s","turn-s"),
 (":turn-w","turn-w"),
 (":turn-e","turn-e"),
 (":turn-nw","turn-nw"),
 (":turn-ne","turn-ne"),
 (":turn-sw","turn-sw"),
 (":turn-se","turn-se"),
 ("x","anachronid"),
 (":anachronid","anachronid"),
 ("a","androsynth"),
 (":androsynth","androsynth"),
 ("A","ascendant"),
 (":ascendant","ascendant"),
 ("c","caduceator"),
 (":caduceator","caduceator"),
 ("e","encephalon"),
 (":encephalon","encephalon"),
 ("g","goliath"),
 (":goliath","goliath"),
 ("h","hellion"),
 (":hellion","hellion"),
 ("k","kraken"),
 (":kraken","kraken"),
 ("m","myrmidon"),
 (":myrmidon","myrmidon"),
 ("p","perennial"),
 (":perennial","perennial"),
 ("r","reptilian"),
 (":reptilian","reptilian"),
 ("R","recreant"),
 (":recreant","recreant"),
 (".","reroll"),
 (":reroll","reroll"),
 ("b","barbarian"),
 (":barbarian","barbarian"),
 ("c","consular"),
 (":consular","consular"),
 ("e","engineer"),
 (":engineer","engineer"),
 ("a","forceadept"),
 (":force-adept","forceadept"),
 ("m","marine"),
 (":marine","marine"),
 ("n","ninja"),
 (":ninja","ninja"),
 ("p","pirate"),
 (":pirate","pirate"),
 ("s","scout"),
 (":scout","scout"),
 ("S","shepherd"),
 (":shepherd","shepherd"),
 ("t","thief"),
 (":thief","thief"),
 ("w","warrior"),
 (":warrior","warrior"),
 ("#quit","quit"),
 (",","pickup"),
 (":pickup","pickup"),
 ("d","drop"),
 (":drop","drop"),
 ("w","wield"),
 (":wield","wield"),
 ("-","unwield"),
 (":unwield","unwield"),
 (">fk","fire-n"),
 (":fire-n","fire-n"),
 (">fj","fire-s"),
 (":fire-s","fire-s"),
 (">fh","fire-w"),
 (":fire-w","fire-w"),
 (">fl","fire-e"),
 (":fire-e","fire-e"),
 (">fy","fire-nw"),
 (":fire-new","fire-nw"),
 (">fu","fire-ne"),
 (":fire-ne","fire-ne"),
 (">fb","fire-sw"),
 (":fire-sw","fire-sw"),
 (">fn","fire-se"),
 (":fire-se","fire-sw"),
 (":continue","continue")]
