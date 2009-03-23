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
     brightness,
     meanBrightness,
     zipRGB3,
     maxRGB,
     minRGB,
     maximizeRGB,
     ColorClass(..),
     addRGB,
     mapRGB,
     invertRGB,
     filterRGB,
     filterRGBLinear,
     scaleRGB,
     rgbToOpenGL,
     rgbaToOpenGL)
    where

import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL.GL.VertexSpec
import RSAGL.AbstractVector
import RSAGL.Interpolation
\end{code}

\texttt{addColor} paints a color on top of another color using the additive color system.  For example, \texttt{red `addColor` green == yellow}.

\texttt{filterColor} paints a color on top of another color using the subtractive color system (actually, multiplicative).  For example, \texttt{yellow `filterColor` green == green}, but \texttt{red `filterColor` green == black}.

\texttt{rgb256} works for colors specified in hexadecimal.  For example, X11 ForestGreen is \texttt{rgb256 0x22 0x8B 0x22}.

\begin{code}
data RGB = RGB { rgb_red, rgb_green, rgb_blue :: !Double }
    deriving (Eq,Show)

data RGBA = RGBA { rgba_a :: !Double, rgba_rgb :: !RGB }
    deriving (Eq,Show)

instance NFData RGB where

instance NFData RGBA where

class (AbstractVector c) => ColorClass c where
    rgb :: Double -> Double -> Double -> c
    rgb256 :: (Integral i) => i -> i -> i -> c
    alpha :: Double -> c -> RGBA
    alpha256 :: (Integral i) => i -> c -> RGBA
    clampColor :: c -> c
    zipRGB :: (Double -> Double -> Double) -> RGB -> c -> c
    zipColor :: (Double -> Double -> Double) -> c -> c -> c
    toPremultipliedRGB :: c -> RGB
    colorToOpenGL :: c -> Color4 Float
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
    toPremultipliedRGB = id
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
    toPremultipliedRGB (RGBA a rgb_color) = scaleRGB a rgb_color
    colorToOpenGL = rgbaToOpenGL
    toRGBA = id
    fromRGB = alpha 1.0

i2f256 :: (Integral i) => i -> Double
i2f256 = (/ 255) . fromIntegral

rgba :: Double -> Double -> Double -> Double -> RGBA
rgba r g b a = alpha a $ (rgb r g b :: RGB)

rgba256 :: (Integral i) => i -> i -> i -> i -> RGBA
rgba256 r g b a = alpha256 a $ (rgb256 r g b :: RGB)

gray :: Double -> RGB
gray x = rgb x x x

gray256 :: (Integral i) => i -> RGB
gray256 x = rgb256 x x x

brightness :: (ColorClass c) => c -> Double
brightness c = 0.2126 * r + 0.7152 * g + 0.0722 * b
    where RGB r g b = toPremultipliedRGB c

meanBrightness :: (ColorClass c) => c -> Double
meanBrightness c = (r + g + b) / 3
    where RGB r g b = toPremultipliedRGB c

zipRGB3 :: (Double -> Double -> Double -> Double) -> RGB -> RGB -> RGB -> RGB
zipRGB3 f (RGB ar ag ab) (RGB br bg bb) (RGB cr cg cb) = RGB (f ar br cr) (f ag bg cg) (f ab bb cb)

maxRGB :: RGB -> Double
maxRGB (RGB r g b) = max r (max g b)

minRGB :: RGB -> Double
minRGB (RGB r g b) = min r (min g b)

maximizeRGB :: RGB -> RGB
maximizeRGB (RGB r g b) | r <= 0 && g <= 0 && b <= 0 = gray 0
maximizeRGB c = mapRGB (/ maxRGB c) c

addRGB :: RGB -> RGB -> RGB
addRGB = addColor

addColor :: (ColorClass c) => c -> c -> c
addColor = zipColor (+)

subColor :: (ColorClass c) => c -> c -> c
subColor = zipColor (-)

{-# INLINE mapRGB #-}
mapRGB :: (Double -> Double) -> RGB -> RGB
mapRGB f (RGB r g b) = RGB (f r) (f g) (f b)

{-# INLINE filterRGB #-}
filterRGB :: RGB -> RGB -> RGB
filterRGB = zipRGB (*)

{-# INLINE scaleRGB #-}
scaleRGB :: Double -> RGB -> RGB
scaleRGB x = mapRGB (*x)

invertRGB :: RGB -> RGB
invertRGB = mapRGB (1-)
\end{code}

\texttt{filterRGBLinear} maps an RGB color between a black point and a white point.  The black point will map to RGB 0 0 0, while
the white point will map to RGB 1 1 1.

\begin{code}
filterRGBLinear :: RGB -> RGB -> RGB -> RGB
filterRGBLinear black_point white_point = zipRGB3 (\b w c -> lerpBetweenClamped (b,c,w) (0,1)) black_point white_point

scaleRGBA :: Double -> RGBA -> RGBA
scaleRGBA x c = c { rgba_a = x * rgba_a c,
                    rgba_rgb = scaleRGB x (rgba_rgb c) }

rgbToOpenGL :: RGB -> Color4 Float
rgbToOpenGL (RGB r g b) = Color4 (realToFrac r :: Float) (realToFrac g :: Float) (realToFrac b :: Float) 1

rgbaToOpenGL :: RGBA -> Color4 Float
rgbaToOpenGL (RGBA a (RGB r g b)) = Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

instance AbstractZero RGB where
    zero = rgb 0 0 0

instance AbstractAdd RGB RGB where
    add = addColor

instance AbstractSubtract RGB RGB where
    sub = subColor

instance AbstractScale RGB where
    scalarMultiply d = scaleRGB d

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
