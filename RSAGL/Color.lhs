\section{Color}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module RSAGL.Color
    (RGB(..),
     RGBA(..),
     rgba,
     rgba256,
     gray,
     gray256,
     ColorClass(..),
     addRGB,
     mapRGB,
     invertRGB,
     filterRGB,
     scaleRGB,
     rgbToOpenGL,
     rgbaToOpenGL)
    where

import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL.GL.VertexSpec
import RSAGL.AbstractVector
\end{code}

\texttt{addColor} paints a color on top of another color using the additive color system.  For example, \texttt{red `addColor` green == yellow}.

\texttt{filterColor} paints a color on top of another color using the subtractive color system (actually, multiplicative).  For example, \texttt{yellow `filterColor` green == green}, but \texttt{red `filterColor` green == black}.

\texttt{rgb256} works for colors specified in hexadecimal.  For example, X11 ForestGreen is \texttt{rgb256 0x22 0x8B 0x22}.

\begin{code}
data RGB = RGB { rgb_red, rgb_green, rgb_blue :: !Float }
    deriving (Eq,Show)

data RGBA = RGBA { rgba_a :: !Float, rgba_rgb :: !RGB }
    deriving (Eq,Show)

instance NFData RGB where

instance NFData RGBA where

class (AbstractVector c) => ColorClass c where
    rgb :: Float -> Float -> Float -> c
    rgb256 :: (Integral i) => i -> i -> i -> c
    alpha :: Float -> c -> RGBA
    alpha256 :: (Integral i) => i -> c -> RGBA
    clampColor :: c -> c
    zipRGB :: (Float -> Float -> Float) -> RGB -> c -> c
    zipColor :: (Float -> Float -> Float) -> c -> c -> c
    brightness :: c -> Float
    colorToOpenGL :: c -> IO ()
    toRGBA :: c -> RGBA
    fromRGB :: RGB -> c

instance ColorClass RGB where
    rgb = RGB
    rgb256 r g b = rgb (i2f256 r) (i2f256 g) (i2f256 b)
    alpha = RGBA
    alpha256 a = alpha (i2f256 a)
    clampColor = mapRGB (min 1 . max 0)
    zipRGB = zipColor
    zipColor f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
    brightness (RGB r g b) = 0.2126 * r + 0.7152 * g + 0.0722 * b
    colorToOpenGL = rgbToOpenGL
    toRGBA = RGBA 1
    fromRGB = id

instance ColorClass RGBA where
    rgb = rgba 1.0
    rgb256 = rgba256 255
    alpha x (RGBA a rgb_color) = RGBA (x*a) rgb_color
    alpha256 i (RGBA a rgb_color) = RGBA ((i2f256 i)*a) rgb_color
    clampColor (RGBA a rgb_color) = RGBA (min 1 $ max 0 a) $ clampColor rgb_color
    zipRGB f x (RGBA a y) = RGBA a $ zipRGB f x y
    zipColor f (RGBA a1 c1) (RGBA a2 c2) = RGBA (f a1 a2) (zipColor f c1 c2)
    brightness (RGBA a rgb_color) = brightness rgb_color * a
    colorToOpenGL = rgbaToOpenGL
    toRGBA = id
    fromRGB = alpha 1.0

i2f256 :: (Integral i) => i -> Float
i2f256 = (/ 255) . fromIntegral

rgba :: Float -> Float -> Float -> Float -> RGBA
rgba r g b a = alpha a $ (rgb r g b :: RGB)

rgba256 :: (Integral i) => i -> i -> i -> i -> RGBA
rgba256 r g b a = alpha256 a $ (rgb256 r g b :: RGB)

gray :: Float -> RGB
gray x = rgb x x x

gray256 :: (Integral i) => i -> RGB
gray256 x = rgb256 x x x

addRGB :: RGB -> RGB -> RGB
addRGB = addColor

addColor :: (ColorClass c) => c -> c -> c
addColor = zipColor (+)

subColor :: (ColorClass c) => c -> c -> c
subColor = zipColor (-)

mapRGB :: (ColorClass c) => (Float -> Float) -> c -> c
mapRGB f = zipRGB (const f) (fromRGB bad_rgb)

bad_rgb :: RGB
bad_rgb = RGB (sqrt (-1)) (sqrt (-1)) (sqrt (-1))

filterRGB :: (ColorClass c) => RGB -> c -> c
filterRGB = zipRGB (*) . fromRGB 

scaleRGB :: (ColorClass c) => Float -> c -> c
scaleRGB x = mapRGB (*x)

invertRGB :: (ColorClass c) => c -> c
invertRGB = mapRGB (1-)

scaleRGBA :: Float -> RGBA -> RGBA
scaleRGBA x c = c { rgba_a = x * rgba_a c,
                    rgba_rgb = scaleRGB x (rgba_rgb c) }

rgbToOpenGL :: RGB -> IO ()
rgbToOpenGL (RGB r g b) = color $! Color4 r g b 1

rgbaToOpenGL :: RGBA -> IO ()
rgbaToOpenGL (RGBA a (RGB r g b)) = color $! Color4 r g b a

instance AbstractZero RGB where
    zero = rgb 0 0 0

instance AbstractAdd RGB RGB where
    add = addColor

instance AbstractSubtract RGB RGB where
    sub = subColor

instance AbstractScale RGB where
    scalarMultiply d = scaleRGB (realToFrac d)

instance AbstractVector RGB

instance AbstractZero RGBA where
    zero = rgba 0 0 0 0

instance AbstractAdd RGBA RGBA where
    add = addColor

instance AbstractSubtract RGBA RGBA where
    sub = subColor

instance AbstractScale RGBA where
    scalarMultiply d = scaleRGBA (realToFrac d)

instance AbstractVector RGBA
\end{code}
