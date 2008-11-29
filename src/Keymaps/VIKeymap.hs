module Keymaps.VIKeymap
    (vi_keymap)
    where

import Keymaps.Keymaps
import Keymaps.CommonKeymap

vi_keymap :: Keymap
vi_keymap = common_keymap ++
    [("k","n"),
     ("j","s"),
     ("h","w"),
     ("l","e"),
     ("y","nw"),
     ("b","sw"),
     ("n","se"),
     ("j","next"),
     ("k","prev")]
