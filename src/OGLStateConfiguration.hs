module OGLStateConfiguration
    (OGLLightConfiguration(..),
     OGLStateConfiguration(..),
     setOpenGLState,
     ogl_bare_bones_configuration,
     ogl_state_configuration_model,
     ogl_state_configuration_effects)
    where

import Graphics.UI.GLUT

data OGLLightConfiguration = OGLLightConfiguration
    { ogl_light_ambient :: Color4 GLfloat,
      ogl_light_diffuse :: Color4 GLfloat,
      ogl_light_position :: Vertex4 GLfloat,
      ogl_light_specular :: Color4 GLfloat
    }

-- |
-- The OGLStateConfiguration data structure stores information about how
-- to set up the OpenGL state machine before drawing.  Other routines
-- may set OpenGL state in undesired ways, and this data structure
-- allows us to specify the state we care about while leaving everything
-- else to some reasonable default.
--
data OGLStateConfiguration = OGLStateConfiguration
    { ogl_background_color :: Color4 Float,
      ogl_depth_func :: Maybe ComparisonFunction,
      ogl_depth_mask :: Capability,
      ogl_shade_model :: ShadingModel,
      ogl_polygon_smooth :: Capability,
      ogl_lighting :: Capability,
      ogl_light_model_ambient :: Color4 Float,
      ogl_light_0 :: Maybe OGLLightConfiguration,
      ogl_light_1 :: Maybe OGLLightConfiguration,
      ogl_light_2 :: Maybe OGLLightConfiguration,
      ogl_light_3 :: Maybe OGLLightConfiguration,
      ogl_fov_degrees :: GLdouble,
      ogl_near_plane :: GLdouble,
      ogl_far_plane :: GLdouble,
      ogl_cull_face :: Maybe Face,
      ogl_blend :: Capability,
      ogl_blend_func :: (BlendingFactor,BlendingFactor),
      ogl_blend_equation :: BlendEquation,
      ogl_fog :: Capability,
      ogl_fog_mode :: FogMode
    }

ogl_bare_bones_configuration :: OGLStateConfiguration
ogl_bare_bones_configuration = OGLStateConfiguration {
						      ogl_background_color = Color4 0 0 0 0,
						      ogl_depth_func = Just Lequal,
						      ogl_depth_mask = Enabled,
						      ogl_shade_model = Smooth,
						      ogl_polygon_smooth = Disabled,
						      ogl_lighting = Disabled,
						      ogl_light_model_ambient = Color4 0 0 0 1,
						      ogl_light_0 = Nothing,
						      ogl_light_1 = Nothing,
						      ogl_light_2 = Nothing,
						      ogl_light_3 = Nothing,
						      ogl_fov_degrees = 90,
						      ogl_near_plane = 1,
						      ogl_far_plane = 10,
						      ogl_cull_face = Nothing,
						      ogl_blend = Disabled,
						      ogl_blend_func = (SrcAlpha,OneMinusSrcAlpha),
						      ogl_blend_equation = FuncAdd,
						      ogl_fog = Disabled,
						      ogl_fog_mode = Linear 1 10
						     }

ogl_state_configuration_model :: OGLStateConfiguration
ogl_state_configuration_model = ogl_bare_bones_configuration { ogl_lighting = Enabled,
							       ogl_cull_face = Just Back }

ogl_state_configuration_effects :: OGLStateConfiguration
ogl_state_configuration_effects = ogl_bare_bones_configuration { ogl_depth_mask = Disabled,
								 ogl_cull_face = Nothing, 
								 ogl_polygon_smooth = Enabled,
								 ogl_blend = Enabled
							       }

setOpenGLState :: OGLStateConfiguration -> IO ()
setOpenGLState config_info = do clearColor $= ogl_background_color config_info
				depthFunc $= ogl_depth_func config_info
				depthMask $= ogl_depth_mask config_info
				lighting $= ogl_lighting config_info
				shadeModel $= ogl_shade_model config_info
				polygonSmooth $= ogl_polygon_smooth config_info
				lightModelAmbient $= ogl_light_model_ambient config_info
				setUpLight 0 $ ogl_light_0 config_info
				setUpLight 1 $ ogl_light_1 config_info
				setUpLight 2 $ ogl_light_2 config_info
				setUpLight 3 $ ogl_light_3 config_info
				blend $= Enabled
				blendEquation $= FuncAdd
				blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
				cullFace $= ogl_cull_face config_info
				blend $= ogl_blend config_info
				blendEquation $= ogl_blend_equation config_info
				blendFunc $= ogl_blend_func config_info
				fog $= ogl_fog config_info
				fogMode $= ogl_fog_mode config_info
				fogColor $= ogl_background_color config_info
				matrixMode $= Projection
				loadIdentity
				(Size width height) <- get windowSize
				perspective (ogl_fov_degrees config_info) ((fromInteger $ toInteger width)/(fromInteger $ toInteger height)) (ogl_near_plane config_info) (ogl_far_plane config_info)
				matrixMode $= Modelview 0
				loadIdentity

setUpLight :: GLint -> Maybe OGLLightConfiguration -> IO ()
setUpLight light_index Nothing = (light $ Light light_index) $= Disabled
setUpLight light_index (Just light_info) = do (light $ Light light_index) $= Enabled
					      (ambient $ Light light_index) $= ogl_light_ambient light_info
					      (specular $ Light light_index) $= ogl_light_specular light_info
					      (diffuse $ Light light_index) $= ogl_light_diffuse light_info
					      (position $ Light light_index) $= ogl_light_position light_info