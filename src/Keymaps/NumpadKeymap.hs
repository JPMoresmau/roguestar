module Keymaps.NumpadKeymap
    (numpad_keymap)
    where

import Keymaps.CommonKeymap
import Keymaps.Keymaps

numpad_keymap :: Keymap
numpad_keymap = common_keymap ++
    [("8","n"),
     ("2","s"),
     ("4","w"),
     ("6","e"),
     ("7","nw"),
     ("9","ne"),
     ("1","sw"),
     ("3","se"),
     ("8","prev"),
     ("2","next")]
