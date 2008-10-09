module Keymaps.CommonKeymap
    (commonMovementKeymap,MovementKeymap(..),common_keymap)
    where

import Data.Char

import Keymaps.Keymaps

data MovementKeymap = MovementKeymap {
    mk_n, mk_s, mk_e, mk_w, mk_nw, mk_ne, mk_sw, mk_se :: String }

commonMovementKeymap :: MovementKeymap -> Keymap
commonMovementKeymap mk = [
 (mk_n mk,"move-n"),
 (mk_s mk,"move-s"),
 (mk_e mk,"move-e"),
 (mk_w mk,"move-w"),
 (mk_nw mk,"move-nw"),
 (mk_ne mk,"move-ne"),
 (mk_sw mk,"move-sw"),
 (mk_se mk,"move-se"),
 (">t" ++ mk_n mk,"turn-n"),
 (">t" ++ mk_s mk,"turn-s"),
 (">t" ++ mk_e mk,"turn-e"),
 (">t" ++ mk_w mk,"turn-w"),
 (">t" ++ mk_nw mk,"turn-nw"),
 (">t" ++ mk_ne mk,"turn-ne"),
 (">t" ++ mk_sw mk,"turn-sw"),
 (">t" ++ mk_se mk,"turn-se"),
 (">f" ++ mk_n mk,"fire-n"),
 (">f" ++ mk_s mk,"fire-s"),
 (">f" ++ mk_e mk,"fire-e"),
 (">f" ++ mk_w mk,"fire-w"),
 (">f" ++ mk_nw mk,"fire-nw"),
 (">f" ++ mk_ne mk,"fire-ne"),
 (">f" ++ mk_sw mk,"fire-sw"),
 (">f" ++ mk_se mk,"fire-se")]

common_keymap :: Keymap
common_keymap = [
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
 (":fire-n","fire-n"),
 (":fire-s","fire-s"),
 (":fire-w","fire-w"),
 (":fire-e","fire-e"),
 (":fire-new","fire-nw"),
 (":fire-ne","fire-ne"),
 (":fire-sw","fire-sw"),
 (":fire-se","fire-sw"),
 (":continue","continue"),
 ("]","zoom-in"),
 (":zoom-in","zoom-in"),
 ("[","zoom-out"),
 (":zoom-out","zoom-out")]
