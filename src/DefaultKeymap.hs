module DefaultKeymap
    (default_keymap)
    where

import Keymaps

default_keymap :: [(String,String)]
default_keymap = fixKeymap [
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
 ("p","pilot"),
 (":pilot","pilot"),
 ("P","privateer"),
 (":privateer","privateer"),
 ("s","scout"),
 (":scout","scout"),
 ("S","shepherd"),
 (":shepherd","shepherd"),
 ("t","thief"),
 (":thief","thief"),
 ("w","warrior"),
 (":warrior","warrior"),
 ("k","move-n"),
 (":move-n","move-n"),
 ("j","move-s"),
 (":move-s","move-s"),
 ("h","move-e"),
 (":move-e","move-e"),
 ("l","move-w"),
 (":move-w","move-w"),
 ("y","move-ne"),
 (":move-ne","move-ne"),
 ("u","move-nw"),
 (":move-nw","move-nw"),
 ("b","move-se"),
 (":move-se","move-se"),
 ("n","move-sw"),
 (":move-sw","move-sw"),
 ("K","turn-n"),
 (":turn-n","turn-n"),
 ("J","turn-s"),
 (":turn-s","turn-s"),
 ("H","turn-e"),
 (":turn-e","turn-e"),
 ("L","turn-w"),
 (":turn-w","turn-w"),
 ("Y","turn-ne"),
 (":turn-se","turn-ne"),
 ("U","turn-nw"),
 (":turn-nw","turn-nw"),
 ("B","turn-se"),
 (":turn-se","turn-se"),
 ("N","turn-sw"),
 (":turn-sw","turn-sw"),
 ("#quit","quit")]
