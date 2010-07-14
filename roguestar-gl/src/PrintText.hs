{-# LANGUAGE OverloadedStrings #-}

-- | A 'PrintTextObject' renders the text console at the bottom of the screen.

module PrintText
    (newPrintTextObject,
     printText,
     setStatus,
     PrintTextObject,
     renderText,
     PrintTextMode(..),
     TextType(..),
     getInputBuffer,
     pushInputBuffer,
     pullInputBuffer,
     setInputBuffer,
     setPrintTextMode,
     clearOutputBuffer,
     clearInputBuffer)
    where

import Control.Concurrent.STM
import Graphics.Rendering.OpenGL as GL
import PrintTextData
import Control.Monad
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import RSAGL.Color
import RSAGL.Color.RSAGLColors
import KeyStroke

data PrintTextData = PrintTextData {
    text_output_buffer :: [(TextType,B.ByteString)],
    text_input_buffer :: B.ByteString,
    text_status_lines :: Map.Map StatusField B.ByteString,
    text_output_mode :: PrintTextMode }

data PrintTextObject = PrintTextObject (TVar PrintTextData) (Chan KeyString)

font_width_pixels :: Int
font_width_pixels = 9

font_height_pixels :: Int
font_height_pixels = 15

padding_pixels :: Int
padding_pixels = 5

font :: GLUT.BitmapFont
font = GLUT.Fixed9By15

newPrintTextObject :: IO PrintTextObject
newPrintTextObject =
    do pt_data <- newTVarIO $ PrintTextData {
           text_output_buffer = [],
           text_input_buffer = B.empty,
           text_status_lines = Map.empty,
           text_output_mode = Unlimited }
       pt_chan <- newChan
       return $ PrintTextObject pt_data pt_chan

printText :: PrintTextObject -> TextType -> B.ByteString -> STM ()
printText (PrintTextObject pto _) text_type str =
    do print_text <- readTVar pto
       writeTVar pto $ print_text {
           text_output_buffer = text_output_buffer print_text ++
                                map ((,) text_type) (B.lines str) }

setStatus :: PrintTextObject -> Map.Map StatusField B.ByteString -> STM ()
setStatus p@(PrintTextObject pto _) status_lines =
    do print_text <- readTVar pto
       let changes = Map.differenceWith
               (\new old -> if new==old then Nothing else Just new)
               status_lines
               (text_status_lines print_text)
       forM_ (Map.toAscList changes) $ \(field,string) ->
           printText p Update $ onChange field string
       writeTVar pto . (\x -> x { text_status_lines = status_lines }) =<< readTVar pto

pushInputBuffer :: PrintTextObject -> KeyStroke -> IO ()
pushInputBuffer (PrintTextObject _ chan) stroke = writeChan chan [stroke]

getInputBuffer :: PrintTextObject -> IO B.ByteString
getInputBuffer (PrintTextObject pto _) =
    liftM text_input_buffer $ atomically $ readTVar pto

-- | Pull one keypress into the input buffer, if it exists.
pullInputBuffer :: PrintTextObject -> IO ()
pullInputBuffer (PrintTextObject pto chan) =
    do e <- isEmptyChan chan
       when (not e) $
           do stroke <- readChan chan
              atomically $
                  do print_text <- readTVar pto
                     writeTVar pto $ print_text {
                         text_input_buffer = text_input_buffer print_text
                             `B.append` B.pack (asString stroke) }
              return ()

setInputBuffer :: PrintTextObject -> B.ByteString -> IO ()
setInputBuffer (PrintTextObject pto _) new_input_buffer = atomically $
    do print_text <- readTVar pto
       writeTVar pto $ print_text { text_input_buffer = new_input_buffer }

clearOutputBuffer :: PrintTextObject -> STM ()
clearOutputBuffer (PrintTextObject pto _) =
    do print_text <- readTVar pto
       writeTVar pto $ print_text { text_output_buffer = [] }

setPrintTextMode :: PrintTextObject -> PrintTextMode -> STM ()
setPrintTextMode (PrintTextObject pto _) pt_mode =
    do print_text <- readTVar pto
       writeTVar pto $ print_text { text_output_mode = pt_mode }

clearInputBuffer :: PrintTextObject -> IO ()
clearInputBuffer (PrintTextObject pto _) = atomically $
    do print_text <- readTVar pto
       writeTVar pto $ print_text { text_input_buffer = B.empty }

renderText :: Size -> PrintTextObject -> IO ()
renderText (Size width height) (PrintTextObject pto _) =
    do ptd <- atomically $ readTVar pto
       let max_characters_height =
               (fromIntegral height - 2 * fromIntegral padding_pixels) `div`
               (fromIntegral font_height_pixels)
       let max_characters_width =
               (fromIntegral width - 2 * fromIntegral padding_pixels) `div`
               (fromIntegral font_width_pixels)
       let lines_to_print =
               restrictLines max_characters_height max_characters_width $
                   (case (text_output_mode ptd) of
                       PrintTextData.Disabled -> []
                       Limited -> reverse $ take 5 $ reverse $
                                      text_output_buffer ptd
                       Unlimited -> (text_output_buffer ptd)) ++
                   (if B.length (text_input_buffer ptd) > 0 ||
                                text_output_mode ptd /= PrintTextData.Disabled
                       then [(Input,"> " `B.append` (text_input_buffer ptd))]
                       else [])
       let status_lines = flip map (Map.toAscList $ text_status_lines ptd) $
               \(field,string) -> (Update,whileActive field string)
       drawLines (Size width height) (reverse lines_to_print) Log
       drawLines (Size width height) (reverse status_lines) Status

data PrintTextPosition = Status | Log

drawLines :: Size -> [(TextType,B.ByteString)] -> PrintTextPosition -> IO ()
drawLines (Size width height) lines_to_print position =
    do save_depth_func <- get depthFunc
       save_depth_mask <- get depthMask
       save_blend <- get blend
       depthFunc $= Nothing
       depthMask $= GL.Disabled
       blend $= Enabled
       matrixMode $= Projection
       loadIdentity
       ortho2D 0 (fromIntegral width) 0 (fromIntegral height)
       matrixMode $= Modelview 0
       loadIdentity
       let actual_width_pixels :: Int
           actual_width_pixels = font_width_pixels *
                                 (maximum $ map (B.length . snd) $ (undefined,""):lines_to_print)
       let actual_height_pixels :: Int
           actual_height_pixels = font_height_pixels * (length lines_to_print)
       let y_position :: Int
           y_position = case position of
               Status -> fromIntegral height - actual_height_pixels - padding_pixels
               Log -> padding_pixels
       color $ (Color4 0 0 0 0.92 :: Color4 GLfloat)
       (rect :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ())
           (Vertex2 (fromIntegral padding_pixels)
                    (fromIntegral y_position))
           (Vertex2 (fromIntegral (actual_width_pixels +
                                   padding_pixels))
                    (fromIntegral (actual_height_pixels +
                                   y_position +
                                   padding_pixels*2)))
       currentRasterPosition $= (Vertex4 (fromIntegral padding_pixels)
                                         (fromIntegral $ y_position + padding_pixels)
                                         0 1)
       mapM_ drawLine lines_to_print
       blend $= save_blend
       depthMask $= save_depth_mask
       depthFunc $= save_depth_func

drawLine :: (TextType,B.ByteString) -> IO ()
drawLine (textType,str) = do (Vertex4 x y _ _) <- get currentRasterPosition
                             color $ colorToOpenGL $ textTypeToColor textType
                             currentRasterPosition $= (Vertex4 x y 0 1)
                             GLUT.renderString font $ B.unpack str
                             currentRasterPosition $= (Vertex4 x (y + fromIntegral font_height_pixels) 0 1)

restrictLines :: Int -> Int -> [(TextType,B.ByteString)] -> [(TextType,B.ByteString)]
restrictLines height width text_lines = reverse $ take height $ reverse $ concatMap splitLongLines text_lines
    where splitLongLines (_,l) | B.null l = []
	  splitLongLines (textType,str) = (textType,B.take width str):(splitLongLines (textType,B.drop width str))

textTypeToColor :: TextType -> RGB
textTypeToColor UnexpectedEvent = orange
textTypeToColor Event =  yellow
textTypeToColor Input =  white
textTypeToColor Query =  grey
textTypeToColor Update = light_brown

