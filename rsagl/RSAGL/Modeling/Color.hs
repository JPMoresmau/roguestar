{-# LANGUAGE MultiParamTypeClasses #-}
module RSAGL.Modeling.Color
    (RGB(..),
     RGBA(..),
     rgba,
     rgba256,
     grayscale,
     greyscale,
     grayscale256,
     greyscale256,
     subjectiveBrightness,
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
import Graphics.Rendering.OpenGL.Raw.Core31 (GLdouble,GLfloat)
import Graphics.Rendering.OpenGL hiding (RGB,RGBA)
import RSAGL.Math.AbstractVector
import RSAGL.Math.Interpolation
import RSAGL.Types

data RGB = RGB { rgb_red, rgb_green, rgb_blue :: !RSdouble }
    deriving (Eq,Show)

data RGBA = RGBA { rgba_a :: !RSdouble, rgba_rgb :: !RGB }
    deriving (Eq,Show)

instance NFData RGB where

instance NFData RGBA where

class (AbstractVector c) => ColorClass c where
    -- | Construct a color from an RGB triple in the range \[0.0..1.0\].
    rgb :: RSdouble -> RSdouble -> RSdouble -> c
    -- | Construct a color from an RGB triple in the range \[0..255\].
    rgb256 :: (Integral i) => i -> i -> i -> c
    -- | Apply an alpha channel to a color, or multiply the existing alpha
    -- channel.  In the rage \[0.0..1.0\].
    alpha :: RSdouble -> c -> RGBA
    -- | Apply an alpha channel to a color, or multiple the existing alpha
    -- channel.  In the range \[0..255\].
    alpha256 :: (Integral i) => i -> c -> RGBA
    -- | Clamp a color within the \[0..1\] range.
    clampColor :: c -> c
    -- | Modify the R, G, and B components of a color.
    zipRGB :: (RSdouble -> RSdouble -> RSdouble) -> RGB -> c -> c
    -- | Modify the R, G, B, and A components of a color.
    zipColor :: (RSdouble -> RSdouble -> RSdouble) -> c -> c -> c
    -- | Get the components of a color, premultiplied by the alpha channel.
    toPremultipliedRGB :: c -> RGB
    -- | Get the OpenGL representation of a color.
    colorToOpenGL :: c -> Color4 GLdouble
    -- | Get the 'RGBA' representation of a color.
    toRGBA :: c -> RGBA
    -- | Import a color from its 'RGB' representation.
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

-- | Convert a value in the range \[0..255\] to the range
-- \[0.0..1.0\].
i2f256 :: (Integral i) => i -> RSdouble
i2f256 = (/ 255) . fromIntegral

-- | Construct an 'RGBA' color from values in the range
-- \[0.0..1.0\].
rgba :: RSdouble -> RSdouble -> RSdouble -> RSdouble -> RGBA
rgba r g b a = alpha a $ (rgb r g b :: RGB)

-- | Construct an 'RGBA' color from values in the range
-- \[0..255\].
rgba256 :: (Integral i) => i -> i -> i -> i -> RGBA
rgba256 r g b a = alpha256 a $ (rgb256 r g b :: RGB)

-- | Construct a gray color from a value in the range
-- \[0.0..1.0\].
grayscale :: RSdouble -> RGB
grayscale = greyscale

greyscale :: RSdouble -> RGB
greyscale x = rgb x x x

-- | Construct a gray color from a value in the range
-- \[0..255\].
grayscale256 :: (Integral i) => i -> RGB
grayscale256 = greyscale256

greyscale256 :: (Integral i) => i -> RGB
greyscale256 x = rgb256 x x x

-- | Calculate the subjective brightness of a color
-- based on each component's apparent brightness.
--
-- * r = 0.2126
-- * g = 0.7152
-- * b = 0.0722
--
subjectiveBrightness :: (ColorClass c) => c -> RSdouble
subjectiveBrightness c = 0.2126 * r + 0.7152 * g + 0.0722 * b
    where RGB r g b = toPremultipliedRGB c

-- | Calculate the average of the RGB color components.
meanBrightness :: (ColorClass c) => c -> RSdouble
meanBrightness c = (r + g + b) / 3
    where RGB r g b = toPremultipliedRGB c

-- | A combining function on three RGB values.
zipRGB3 :: (RSdouble -> RSdouble -> RSdouble -> RSdouble) -> RGB -> RGB -> RGB -> RGB
zipRGB3 f (RGB ar ag ab) (RGB br bg bb) (RGB cr cg cb) = RGB (f ar br cr) (f ag bg cg) (f ab bb cb)

-- | Get the maximum of the three RGB components.
maxRGB :: RGB -> RSdouble
maxRGB (RGB r g b) = max r (max g b)

-- | Get the minimum of the three RGB components.
minRGB :: RGB -> RSdouble
minRGB (RGB r g b) = min r (min g b)

-- | Maximize an RGB color to the brightest matching color in gamut.
maximizeRGB :: RGB -> RGB
maximizeRGB (RGB r g b) | r <= 0 && g <= 0 && b <= 0 = grayscale 0
maximizeRGB c = mapRGB (/ maxRGB c) c

-- | Component-wise addition of two RGB values.
addRGB :: RGB -> RGB -> RGB
addRGB = addColor

-- | Component-wise addition of two colors.
addColor :: (ColorClass c) => c -> c -> c
addColor = zipColor (+)

-- | Component-wise subtraction of two colors.
subColor :: (ColorClass c) => c -> c -> c
subColor = zipColor (-)

-- | Component-wise modification of a color.
{-# INLINE mapRGB #-}
mapRGB :: (RSdouble -> RSdouble) -> RGB -> RGB
mapRGB f (RGB r g b) = RGB (f r) (f g) (f b)

-- | Component-wise multiplication of two colors.
{-# INLINE filterRGB #-}
filterRGB :: RGB -> RGB -> RGB
filterRGB = zipRGB (*)

-- | Scalar multiplication of a color.
{-# INLINE scaleRGB #-}
scaleRGB :: RSdouble -> RGB -> RGB
scaleRGB x = mapRGB (*x)

-- | Arithmetic inverse of a color.
invertRGB :: RGB -> RGB
invertRGB = mapRGB (1-)

-- | maps an RGB color between a black point and a white point.
-- The first parameter, the black point, will map to RGB 0 0 0.
-- The second parameter, the white point, will map to RGB 1 1 1.
filterRGBLinear :: RGB -> RGB -> RGB -> RGB
filterRGBLinear black_point white_point = zipRGB3 (\b w c -> lerpBetweenClamped (b,c,w) (0,1)) black_point white_point

-- | Component-wise scalar multiplication of a color.
scaleRGBA :: RSdouble -> RGBA -> RGBA
scaleRGBA x c = c { rgba_a = x * rgba_a c,
                    rgba_rgb = scaleRGB x (rgba_rgb c) }

-- | Get the OpenGL representation of a color.
rgbToOpenGL :: RGB -> Color4 GLdouble
rgbToOpenGL (RGB r g b) = Color4 (toGLdouble r) (toGLdouble g) (toGLdouble b) 1

-- | Get the OpenGL representation of a color.
rgbaToOpenGL :: RGBA -> Color4 GLdouble
rgbaToOpenGL (RGBA a (RGB r g b)) = Color4 (toGLdouble r) (toGLdouble g) (toGLdouble b) (toGLdouble a)

instance AbstractZero RGB where
    zero = rgb 0 0 0

instance AbstractAdd RGB RGB where
    add = addColor

instance AbstractSubtract RGB RGB where
    sub = subColor

instance AbstractScale RGB where
    scalarMultiply d = scaleRGB $ f2f d

instance AbstractVector RGB

instance AbstractZero RGBA where
    zero = rgba 0 0 0 0

instance AbstractAdd RGBA RGBA where
    add = addColor

instance AbstractSubtract RGBA RGBA where
    sub = subColor

instance AbstractScale RGBA where
    scalarMultiply d = scaleRGBA $ f2f d

instance AbstractVector RGBA

