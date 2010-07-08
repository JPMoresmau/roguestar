{-# LANGUAGE FlexibleInstances #-}
module RSAGL.Color.OpenGL
    (ColorToOpenGL(..))
    where

import RSAGL.Math.Types
import RSAGL.Color.RGB
import RSAGL.Color.Alpha
import Graphics.Rendering.OpenGL.Raw.Core31 (GLdouble,GLfloat)
import Graphics.Rendering.OpenGL hiding (RGB,RGBA,Alpha)

class ColorToOpenGL c where
    colorToOpenGL :: c -> Color4 GLdouble

instance ColorToOpenGL RGB where
    colorToOpenGL (RGB r g b) = Color4 (toGLdouble r)
                                       (toGLdouble g)
                                       (toGLdouble b)
                                       1

instance ColorToOpenGL (Alpha RGB) where
    colorToOpenGL (Alpha a (RGB r g b)) = Color4 (toGLdouble r)
                                                 (toGLdouble g)
                                                 (toGLdouble b)
                                                 (toGLdouble a)

