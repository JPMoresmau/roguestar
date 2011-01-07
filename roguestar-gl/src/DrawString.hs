{-# LANGUAGE CPP #-}
module DrawString
    (DrawString(..),
     stderrDrawString,
     glutDrawString
     )
    where

import System.IO

import qualified Graphics.UI.GLUT as GLUT

data DrawString = DrawString {
    draw_string_font_width :: Int,
    draw_string_font_height :: Int,
    draw_string_padding :: Int,
    drawString :: String -> IO () }

stderrDrawString :: DrawString
stderrDrawString = DrawString {
    draw_string_font_width = 0,
    draw_string_font_height = 0,
    draw_string_padding = 0,
    drawString = hPutStrLn stderr }

glutDrawString :: DrawString
glutDrawString = DrawString {
    draw_string_font_width = 9,
    draw_string_font_height = 15,
    draw_string_padding = 5,
    drawString = GLUT.renderString GLUT.Fixed9By15 }
