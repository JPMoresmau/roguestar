\section{Color}

\begin{code}
module RSAGL.Color
    (RGB(..),
     RGBA(..),
     rgba,
     rgba256,
     ColorClass(..),
     addRGB,
     scaleRGB)
    where

import RSAGL.Interpolation
\end{code}

\texttt{addColor} paints a color on top of another color using the additive color system.  For example, \texttt{red `addColor` green == yellow}.

\texttt{filterColor} paints a color on top of another color using the subtractive color system (actually, multiplicative).  For example, \texttt{yellow `filterColor` green == green}, but \texttt{red `filterColor` green == black}.

\texttt{rgb256} works for colors specified in hexadecimal.  For example, X11 ForestGreen is \texttt{rgb256 0x22 0x8B 0x22}.

\begin{code}
data RGB = RGB { rgb_red, rgb_green, rgb_blue :: Float }
    deriving (Eq,Show)

data RGBA = RGBA { rgba_a :: Float, rgba_rgb :: RGB }
    deriving (Eq,Show)

class ColorClass c where
    rgb :: Float -> Float -> Float -> c
    rgb256 :: (Integral i) => i -> i -> i -> c
    alpha :: Float -> c -> RGBA
    alpha256 :: (Integral i) => i -> c -> RGBA
    lerpColor :: Float -> (c,c) -> c
    clampColor :: c -> c
    mapRGB :: (Float -> Float) -> c -> c
    brightness :: c -> Float

instance ColorClass RGB where
    rgb = RGB
    rgb256 r g b = rgb (i2f256 r) (i2f256 g) (i2f256 b)
    alpha = RGBA
    alpha256 a = alpha (i2f256 a)
    lerpColor u (a,b) = addRGB (scaleRGB (1-u) a) (scaleRGB u b)
    clampColor = mapRGB (min 1 . max 0)
    mapRGB f (RGB r g b) = RGB (f r) (f g) (f b)
    brightness (RGB r g b) = 0.2126 * r + 0.7152 * g + 0.0722 * b

instance ColorClass RGBA where
    rgb = rgba 1.0
    rgb256 = rgba256 255
    alpha x (RGBA a rgb_color) = RGBA (x*a) rgb_color
    alpha256 i (RGBA a rgb_color) = RGBA ((i2f256 i)*a) rgb_color
    lerpColor u (RGBA a1 rgb1,RGBA a2 rgb2) = RGBA (lerpNumber u (a1,a2)) (lerpColor u (rgb1,rgb2))
    clampColor (RGBA a rgb_color) = RGBA (min 1 $ max 0 a) $ clampColor rgb_color
    mapRGB f (RGBA a rgb_color) = RGBA a $ mapRGB f rgb_color
    brightness (RGBA a rgb_color) = brightness rgb_color * a

i2f256 :: (Integral i) => i -> Float
i2f256 = (/ 255) . fromIntegral

rgba :: Float -> Float -> Float -> Float -> RGBA
rgba r g b a = alpha a $ (rgb r g b :: RGB)

rgba256 :: (Integral i) => i -> i -> i -> i -> RGBA
rgba256 r g b a = alpha256 a $ (rgb256 r g b :: RGB)

zipRGB :: (Float -> Float -> Float) -> RGB -> RGB -> RGB
zipRGB f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)

addRGB :: RGB -> RGB -> RGB
addRGB = zipRGB (+)

scaleRGB :: (ColorClass c) => Float -> c -> c
scaleRGB x = mapRGB (*x)

instance Lerpable RGB where
    lerp u = lerpColor (realToFrac u)

instance Lerpable RGBA where
    lerp u = lerpColor (realToFrac u)
\end{code}