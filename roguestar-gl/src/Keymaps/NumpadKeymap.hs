{-# LANGUAGE OverloadedStrings #-}

module Keymaps.NumpadKeymap
    (numpad_keymap)
    where

import Keymaps.CommonKeymap
import Keymaps.Keymaps

numpad_keymap :: Keymap
numpad_keymap = common_keymap ++
    [(">&NumPad8;","n"),
     (">&NumPad2;","s"),
     (">&NumPad4;","w"),
     (">&NumPad6;","e"),
     (">&NumPad7;","nw"),
     (">&NumPad9;","ne"),
     (">&NumPad1;","sw"),
     (">&NumPad3;","se"),
     (">&NumPad8;","prev"),
     (">&NumPad2;","next")]
