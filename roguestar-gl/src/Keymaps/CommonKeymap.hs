{-# LANGUAGE OverloadedStrings #-}

module Keymaps.CommonKeymap
    (common_keymap)
    where

import PrintTextData
import Keymaps.Keymaps

common_keymap :: Keymap
common_keymap = [
 (":north","n"),
 (":northeast","ne"),
 (":east","e"),
 (":southeast","sw"),
 (":south","s"),
 (":southwest","sw"),
 (":west","w"),
 (":northwest","nw"),
 (":escape","escape"),
 (":next","next"),
 (":prev","prev"),
 (":down","down"),
 (":up","up"),
 (">","down"),
 ("<","up"),
 ("&KeyActivate;","select-menu"),
 ("&KeyDown;","next"),
 ("&KeyUp;","prev"),
 ("&KeyEscape;","normal"),
 (":move","move"),
 ("t","turn"),
 (":turn","turn"),
 ("J","jump"),
 (":jump","jump"),
 ("F","attack"),
 (":attack","attack"),
 ("f","fire"),
 (":fire","fire"),
 ("c","clear-terrain"),
 (":clear-terrain","clear-terrain"),
 ("a","activate"),
 (":activate","activate"),
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
 ("m","make-begin"),
 ("p","pistol"),
 (":pistol","pistol"),
 ("c","carbine"),
 (":carbine","carbine"),
 ("r","rifle"),
 (":rifle","rifle"),
 ("f","fleuret"),
 (":fleuret","fleuret"),
 ("s","sabre"),
 (":sabre","sabre"),
 ("&KeyActivate;","make-end"),
 ("#quit","quit"),
 ("#sky-on","sky-on"),
 ("#sky-off","sky-off"),
 ("#quality-1","quality-bad"),
 ("#quality-2","quality-poor"),
 ("#quality-3","quality-good"),
 ("#quality-4","quality-super"),
 (",","pickup"),
 (":pickup","pickup"),
 ("d","drop"),
 (":drop","drop"),
 ("w","wield"),
 (":wield","wield"),
 ("-","unwield"),
 (":unwield","unwield"),
 (":continue","continue"),
 ("]","zoom-in"),
 (":zoom-in","zoom-in"),
 ("[","zoom-out"),
 (":zoom-out","zoom-out")]
