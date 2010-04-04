-- | A 'PrintTextObject' renders the text console at the bottom of the screen.

module PrintText
    (newPrintTextObject,
     printText,
     PrintTextObject,
     renderText,
     PrintTextMode(..),
     TextType(..),
     getInputBuffer,
     pullInputBuffer,
     setInputBuffer,
     setPrintTextMode,
     clearOutputBuffer,
     clearInputBuffer,
     keyCallback)
    where

import Data.IORef
import Graphics.UI.GLUT as GLUT
import PrintTextData
import Control.Monad
import Control.Concurrent.Chan
import System.IO

data PrintTextData = PrintTextData {
    text_output_buffer :: [(TextType,String)],
    text_input_buffer :: String,
    text_output_mode :: PrintTextMode }

data PrintTextObject = PrintTextObject (IORef PrintTextData) (Chan String)

font_width_pixels :: Int
font_width_pixels = 9

font_height_pixels :: Int
font_height_pixels = 15

padding_pixels :: Int
padding_pixels = 10

font :: BitmapFont
font = Fixed9By15

newPrintTextObject :: IO PrintTextObject
newPrintTextObject = 
    do pt_data <- newIORef $ PrintTextData {
           text_output_buffer = [],
           text_input_buffer = [],
           text_output_mode = Unlimited } 
       pt_chan <- newChan
       return $ PrintTextObject pt_data pt_chan

printText :: PrintTextObject -> TextType -> String -> IO ()
printText (PrintTextObject pto _) text_type str =
    modifyIORef pto $ 
        \print_text -> print_text { text_output_buffer = text_output_buffer print_text ++ map (\l -> (text_type,l)) (lines str) } 

keyCallback :: PrintTextObject -> KeyboardMouseCallback
keyCallback (PrintTextObject _ chan) (Char char) Up _ _ = writeChan chan [char]
keyCallback (PrintTextObject _ chan) (SpecialKey special) Up _ _ = writeChan chan $ ":::" ++ show special
keyCallback _ _ _ _ _ = return ()

getInputBuffer :: PrintTextObject -> IO String
getInputBuffer (PrintTextObject pto _) = liftM text_input_buffer $ readIORef pto

-- | Pull one keypress into the input buffer, if it exists.
pullInputBuffer :: PrintTextObject -> IO ()
pullInputBuffer (PrintTextObject pto chan) =
    do e <- isEmptyChan chan
       when (not e) $
           do r <- readChan chan
              modifyIORef pto $ \print_text -> print_text { text_input_buffer = text_input_buffer print_text ++ r }
              return ()

setInputBuffer :: PrintTextObject -> String -> IO ()
setInputBuffer (PrintTextObject pto _) new_input_buffer = modifyIORef pto $
    \print_text -> print_text { text_input_buffer = new_input_buffer }

clearOutputBuffer :: PrintTextObject -> IO ()
clearOutputBuffer (PrintTextObject pto _) = modifyIORef pto $ \ptd -> ptd { text_output_buffer = [] }

setPrintTextMode :: PrintTextObject -> PrintTextMode -> IO ()
setPrintTextMode (PrintTextObject pto _) pt_mode =
    do modifyIORef pto $ \print_text -> print_text { text_output_mode = pt_mode }

clearInputBuffer :: PrintTextObject -> IO ()
clearInputBuffer (PrintTextObject pto _) = modifyIORef pto $ \ptd -> ptd { text_input_buffer = [] }

renderText :: PrintTextObject -> IO ()
renderText (PrintTextObject pto _) = 
    do ptd <- readIORef pto
       (Size width height) <- get windowSize
       save_depth_func <- get depthFunc
       save_depth_mask <- get depthMask
       save_blend <- get blend
       depthFunc $= Nothing
       depthMask $= GLUT.Disabled
       blend $= Enabled
       matrixMode $= Projection
       loadIdentity
       ortho2D 0 (fromIntegral width) 0 (fromIntegral height)
       matrixMode $= Modelview 0
       loadIdentity
       let max_characters_height = (fromIntegral height - 2 * fromIntegral padding_pixels) `div` (fromIntegral font_height_pixels)
       let max_characters_width = (fromIntegral width - 2 * fromIntegral padding_pixels) `div` (fromIntegral font_width_pixels)
       let lines_to_print = restrictLines max_characters_height max_characters_width $ 
			    (case (text_output_mode ptd) of
			        PrintTextData.Disabled -> []
			        Limited -> reverse $ take 3 $ reverse $ text_output_buffer ptd
	        		Unlimited -> (text_output_buffer ptd)) ++
                            (if length (text_input_buffer ptd) > 0 || (text_output_mode ptd) /= PrintTextData.Disabled 
			        then [(Query,"> " ++ (text_input_buffer ptd))] 
			        else [])
	   actual_width_pixels = font_width_pixels * (maximum $ map (length . snd) lines_to_print)
	   actual_height_pixels = font_height_pixels * (length lines_to_print)
	   in do color $ (Color4 0 0 0 0.92 :: Color4 GLfloat)
		 (rect :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ())
		     (Vertex2 (fromIntegral padding_pixels) (fromIntegral padding_pixels)) 
		     (Vertex2 (fromIntegral (actual_width_pixels + padding_pixels)) (fromIntegral (actual_height_pixels + padding_pixels)))
		 currentRasterPosition $= (Vertex4 (fromIntegral padding_pixels) (fromIntegral padding_pixels) 0 1)
		 mapM_ drawLine $ reverse lines_to_print
       blend $= save_blend
       depthMask $= save_depth_mask
       depthFunc $= save_depth_func

drawLine :: (TextType,String) -> IO ()
drawLine (textType,str) = do (Vertex4 x y _ _) <- get currentRasterPosition
			     color $ textTypeToColor textType
			     currentRasterPosition $= (Vertex4 x y 0 1)
			     renderString font str
			     currentRasterPosition $= (Vertex4 x (y + fromIntegral font_height_pixels) 0 1)

restrictLines :: Int -> Int -> [(TextType,String)] -> [(TextType,String)]
restrictLines height width text_lines = reverse $ take height $ reverse $ concatMap splitLongLines text_lines
    where splitLongLines (_,[]) = []
	  splitLongLines (textType,str) = (textType,take width str):(splitLongLines (textType,drop width str))

textTypeToColor :: TextType -> Color3 GLfloat
textTypeToColor UnexpectedEvent = Color3 1.0 0.5 0.0
textTypeToColor Event = Color3 0.5 0.75 1.0
textTypeToColor Input = Color3 0.5 1.0 0.5
textTypeToColor Query = Color3 1.0 1.0 1.0
