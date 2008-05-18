module Keymaps.VIKeymap
    (vi_movement_keymap,vi_keymap)
    where

import Keymaps.Keymaps
import Keymaps.CommonKeymap

vi_movement_keymap :: MovementKeymap
vi_movement_keymap = MovementKeymap {
    mk_n = "k",
    mk_s = "j",
    mk_w = "h",
    mk_e = "l",
    mk_nw = "y",
    mk_ne = "u",
    mk_sw = "b",
    mk_se = "n" }

vi_keymap :: Keymap
vi_keymap =
   commonMovementKeymap vi_movement_keymap ++ common_keymap
