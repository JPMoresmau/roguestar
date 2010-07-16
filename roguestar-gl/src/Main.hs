{-# LANGUAGE CPP #-}
module Main
    (main)
    where

import System.Exit
import System.IO

#ifdef __ROGUESTAR_SUPPORTS_GLUT__
import MainGLUT
#endif

#ifdef __ROGUESTAR_SUPPORTS_GTK__
import MainGTK
#endif

main :: IO ()
main = do
#ifdef __ROGUESTAR_SUPPORTS_GTK__
    mainGTK
    exitWith ExitSuccess
#endif
#ifdef __ROGUESTAR_SUPPORTS_GLUT__
    mainGLUT
    exitWith ExitSuccess
#endif
    hPutStrLn stderr "roguestar-gl was apparently not compiled with any GUI support!"
    exitWith (ExitFailure 1)
