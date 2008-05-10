module Keymaps.BuiltinKeymaps
    (findKeymapOrDefault,
     builtin_keymap_names,
     builtin_keymaps)
    where

import Data.Char
import Data.List

import Keymaps.Keymaps
import Keymaps.NumpadKeymap
import Keymaps.VIKeymap

findKeymapOrDefault :: Maybe KeymapName -> Keymap
findKeymapOrDefault m_keymap_name = fixKeymap $ maybe vi_numpad_keymap id (m_keymap_name >>= (flip lookup builtin_keymaps . (map toLower)))

builtin_keymap_names :: [KeymapName]
builtin_keymap_names = map fst builtin_keymaps

builtin_keymaps :: [(KeymapName, Keymap)]
builtin_keymaps = [
 ("vi", vi_keymap),
 ("numpad", numpad_keymap),
 ("vi+numpad", vi_numpad_keymap)]

vi_numpad_keymap :: Keymap
vi_numpad_keymap = union vi_keymap numpad_keymap
