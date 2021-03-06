module RSAGL.Modeling.BakedModel
    (BakedSurface,
     bakeSurface,
     freeSurface,
     surfaceToOpenGL,
     tesselatedElementToOpenGL)
    where

import Foreign
import Graphics.Rendering.OpenGL.GL
import RSAGL.Modeling.OpenGLPrimitives
import RSAGL.Modeling.Tesselation hiding (tesselatedElementToOpenGL)
import Control.Monad

data BakedFragment = BakedFragment {
    baked_model_primitive_mode :: PrimitiveMode,
    baked_model_length :: GLsizei,
    baked_model_vertex_ptr :: Ptr (Vertex3 GLdouble),
    baked_model_normal_ptr :: Ptr (Normal3 GLdouble),
    baked_model_color_ptr :: Maybe (Ptr (Color4 GLdouble)) }

data BakedSurface = BakedSurface {
    baked_model_action :: IO () -> IO (),
    baked_model_fragments :: [BakedFragment] }

bakeFragment :: (OpenGLPrimitive a) => PrimitiveMode -> Bool -> [a] -> IO BakedFragment
bakeFragment primitive_mode colors_on as =
    do let l = length as
       v <- newArray $ map getVertex as
       n <- newArray $ map getNormal as
       m_c <- case colors_on of
           False -> return Nothing
           True -> liftM Just $ newArray $ map getColor as
       return $ BakedFragment {
           baked_model_primitive_mode = primitive_mode,
           baked_model_length = fromInteger $ toInteger l,
           baked_model_vertex_ptr = v,
           baked_model_normal_ptr = n,
           baked_model_color_ptr = m_c }

freeFragment :: BakedFragment -> IO ()
freeFragment baked_run =
    do free $ baked_model_vertex_ptr baked_run
       free $ baked_model_normal_ptr baked_run
       maybe (return ()) free $ baked_model_color_ptr baked_run

fragmentToOpenGL :: BakedFragment -> IO ()
fragmentToOpenGL baked_fragment =
    do arrayPointer VertexArray $= VertexArrayDescriptor 3 Double 0 (baked_model_vertex_ptr baked_fragment)
       arrayPointer NormalArray $= VertexArrayDescriptor 3 Double 0 (baked_model_normal_ptr baked_fragment)
       flip (maybe $ return ()) (baked_model_color_ptr baked_fragment) $ \color_ptr -> arrayPointer ColorArray $= VertexArrayDescriptor 4 Double 0 color_ptr
       drawArrays (baked_model_primitive_mode baked_fragment) 0 (baked_model_length baked_fragment)

bakeSurface :: (OpenGLPrimitive a) => (IO () -> IO ()) -> Bool -> [(PrimitiveMode,[a])] -> IO BakedSurface
bakeSurface wrapperM colors_on fragment_data =
    do fragments <- mapM (\(primitive_mode,a) -> bakeFragment primitive_mode colors_on a) fragment_data
       return $ BakedSurface {
           baked_model_action = \actionM -> wrapperM $
               do save_vertex_arrays <- get $ clientState VertexArray
                  save_normal_arrays <- get $ clientState NormalArray
                  save_color_arrays <- get $ clientState ColorArray
                  clientState VertexArray $= Enabled
                  clientState NormalArray $= Enabled
                  clientState ColorArray $= if colors_on then Enabled else Disabled
                  actionM
                  clientState ColorArray $= save_color_arrays
                  clientState NormalArray $= save_normal_arrays
                  clientState VertexArray $= save_vertex_arrays,
           baked_model_fragments = fragments }

freeSurface :: BakedSurface -> IO ()
freeSurface baked_surface = mapM_ freeFragment $ baked_model_fragments baked_surface

surfaceToOpenGL :: BakedSurface -> IO ()
surfaceToOpenGL baked_surface = baked_model_action baked_surface $ mapM_ fragmentToOpenGL $ baked_model_fragments baked_surface

tesselatedElementToOpenGL :: (OpenGLPrimitive a) => Bool -> TesselatedElement a -> IO ()
tesselatedElementToOpenGL colors_on tesselated_element =
    do fragment <- bakeSurface id colors_on $ [unmapTesselatedElement tesselated_element]
       surfaceToOpenGL fragment
       freeSurface fragment
