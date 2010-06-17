module RSAGL.Modeling.OpenGLPrimitives
    (OpenGLPrimitive(..),
     renderPrimitives)
    where

import Graphics.Rendering.OpenGL.GL
import Control.Monad

class OpenGLPrimitive a where
    getVertex :: a -> Vertex3 GLdouble
    getNormal :: a -> Normal3 GLdouble
    getColor :: a -> Color4 GLdouble

-- | Simple 'renderPrimitive' function for a list of vertices.  Optionally a single color can be specified to override vertex-wise colors.
renderPrimitives :: (OpenGLPrimitive a) => PrimitiveMode -> Bool -> [a] -> IO ()
renderPrimitives mode colors_on as = unsafeRenderPrimitive mode $ mapM_ renderElement as
    where renderElement a =
              do when colors_on $ color $ getColor a
                 normal $ getNormal a
                 vertex $ getVertex a
