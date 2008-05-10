module Keymaps.NumpadKeymap
    (numpad_movement_keymap,numpad_keymap)
    where

import Data.Char
import Data.Maybe

import Keymaps.CommonKeymap
import Keymaps.Keymaps

numpad_movement_keymap :: MovementKeymap
numpad_movement_keymap = MovementKeymap {
    mk_n = "8",
    mk_s = "2",
    mk_w = "4",
    mk_e = "6",
    mk_nw = "7",
    mk_ne = "9",
    mk_sw = "1",
    mk_se = "3" }

numpad_keymap :: Keymap
numpad_keymap =
   commonMovementKeymap numpad_movement_keymap ++ common_keymap
