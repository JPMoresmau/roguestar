module PrintText
    (printText,
     renderText,
     PrintTextMode(..),
     TextType(..),
     printTranslated,
     clearText,
     text_break)
    where

import Data.IORef
import Graphics.UI.GLUT as GLUT
import Globals
import OGLStateConfiguration
import PrintTextData
import Translation
import Control.Monad

font_width_pixels :: Int
font_width_pixels = 9

font_height_pixels :: Int
font_height_pixels = 15

padding_pixels :: Int
padding_pixels = 10

font :: BitmapFont
font = Fixed9By15

text_break :: String
text_break = "----"

printText :: IORef RoguestarGlobals -> TextType -> String -> IO ()
printText globals_ref textType str =
    modifyIORef globals_ref $ 
		    ( \ globals -> globals { 
					    global_text_output_buffer = (takeWhile ((/= text_break) . snd) $
									 (map ( \ x -> (textType,x)) 
									  (reverse $ lines str)) ++
									 (global_text_output_buffer globals)) } )

clearText :: IORef RoguestarGlobals -> IO ()
clearText globals_ref = modifyIORef globals_ref (\globals -> globals { global_text_output_buffer = [] } )

renderText :: IORef RoguestarGlobals -> IO ()
renderText globals_ref = 
    do user_input <- liftM global_user_input $ readIORef globals_ref
       text_output <- liftM global_text_output_buffer $ readIORef globals_ref
       text_output_mode <- liftM global_text_output_mode $ readIORef globals_ref
       (Size width height) <- get windowSize
       setOpenGLState ogl_bare_bones_configuration { ogl_depth_func = Nothing, ogl_depth_mask = GLUT.Disabled, ogl_blend = Enabled }
       matrixMode $= Projection
       loadIdentity
       ortho2D 0 (fromIntegral width) 0 (fromIntegral height)
       matrixMode $= Modelview 0
       loadIdentity
       let max_characters_height = (fromIntegral height - 2 * fromIntegral padding_pixels) `div` (fromIntegral font_height_pixels)
	   max_characters_width = (fromIntegral width - 2 * fromIntegral padding_pixels) `div` (fromIntegral font_width_pixels)
	   lines_to_print = restrictLines max_characters_height max_characters_width $ 
			    (if length user_input > 0 || text_output_mode /= PrintTextData.Disabled 
			     then [(GUIMessage,"> " ++ user_input)] 
			     else []) ++
			    case (text_output_mode) of
						    PrintTextData.Disabled -> []
					            Limited -> take 3 text_output
	        				    Unlimited -> text_output
	   actual_width_pixels = font_width_pixels * (maximum $ map (length . snd) lines_to_print)
	   actual_height_pixels = font_height_pixels * (length lines_to_print)
	   in do color $ (Color4 0 0 0 0.92 :: Color4 GLfloat)
		 (rect :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ())
		     (Vertex2 (fromIntegral padding_pixels) (fromIntegral padding_pixels)) 
		     (Vertex2 (fromIntegral (actual_width_pixels + padding_pixels)) (fromIntegral (actual_height_pixels + padding_pixels)))
		 currentRasterPosition $= (Vertex4 (fromIntegral padding_pixels) (fromIntegral padding_pixels) 0 1)
		 mapM_ drawLine lines_to_print

drawLine :: (TextType,String) -> IO ()
drawLine (textType,str) = do (Vertex4 x y _ _) <- get currentRasterPosition
			     color $ textTypeToColor textType
			     currentRasterPosition $= (Vertex4 x y 0 1)
			     renderString font str
			     currentRasterPosition $= (Vertex4 x (y + fromIntegral font_height_pixels) 0 1)

restrictLines :: Int -> Int -> [(TextType,String)] -> [(TextType,String)]
restrictLines height width text_lines = take height $ concatMap (reverse . splitLongLines) text_lines
    where splitLongLines (_,[]) = []
	  splitLongLines (textType,str) = (textType,take width str):(splitLongLines (textType,drop width str))

textTypeToColor :: TextType -> Color3 GLfloat
textTypeToColor Untranslated = Color3 1.0 0.5 0.0
textTypeToColor Information = Color3 0.5 0.75 1.0
textTypeToColor UserQuery = Color3 0.5 1.0 0.5
textTypeToColor GUIMessage = Color3 1.0 1.0 1.0

-- |
-- Prints the translated data.
--
printTranslated :: IORef RoguestarGlobals -> TextType -> [String] -> IO ()
printTranslated globals_ref ttype args = printText globals_ref ttype =<< liftM (flip translateStr args . global_language) (readIORef globals_ref)
