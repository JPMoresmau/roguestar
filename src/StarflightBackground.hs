module StarflightBackground
    (renderStarflightBackground,
     renderStarflightFlyby)
    where

--import Graphics.Rendering.OpenGL.GL
--import Graphics.Rendering.OpenGL.GLU
import System.Time
import Graphics.UI.GLUT
import System.Random
import Data.List
import Model

-- |
-- Renders an animated background as though traveling through hyperspace.
-- Note: Clears the color buffer and sets OpenGL state.
--
renderStarflightBackground :: IO ()
renderStarflightBackground = do clearColor $= Color4 0 0 0 0
				clear [ColorBuffer,DepthBuffer]
				depthFunc $= Nothing
				depthMask $= Disabled
				shadeModel $= Smooth
				lighting $= Disabled
				polygonSmooth $= Enabled
				blend $= Enabled
				blendEquation $= FuncAdd
				blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
				fog $= Enabled
				fogColor $= (Color4 0 0 0 1)
				fogMode $= Linear 500 1000
				matrixMode $= Projection
				loadIdentity
				(Size width height) <- get windowSize
				perspective 90.0 ((fromInteger $ toInteger width)/(fromInteger $ toInteger height)) 1 1000
				matrixMode $= Modelview 0
				loadIdentity
				rotate_cycle <- cycleSeconds 360
				cyc <- cycleSeconds 7
				lookAt (Vertex3 0 0 (fromRational $ toRational cyc * 1000)) (Vertex3 0 0 2000) (Vector3 (fromRational $ toRational $ sin $ 2*pi*rotate_cycle) (fromRational $ toRational $ cos $ 2*pi*rotate_cycle) 0)
				preservingMatrix renderStarsFar
				preservingMatrix renderStarsNear
				preservingMatrix renderStarsBehind
				polygonSmooth $= Disabled
				    where renderStarsNear = do renderStars (-1000,1000) (-1000,1000) (1,1000) starfield
					  renderStarsFar = do translate $ (Vector3 0 0 1000 :: Vector3 Float)
							      renderStarsNear
					  renderStarsBehind = do translate $ (Vector3 0 0 (-1000) :: Vector3 Float)
								 renderStarsNear

-- |
-- Renders a model (presumably a starship) traveling past the camera through hyperspace.
--
renderStarflightFlyby :: Float -> Model -> IO ()
renderStarflightFlyby _ model =
    do renderStarflightBackground
       depthFunc $= Just Lequal
       depthMask $= Enabled
       shadeModel $= Smooth
       lighting $= Enabled
       blend $= Disabled
       lightModelAmbient $= (Color4 0.25 0.25 0.25 0)
       light (Light 0) $= Enabled
       ambient (Light 0) $= Color4 0 0 0 0
       diffuse (Light 0) $= Color4 0 0 0.15 0
       specular (Light 0) $= Color4 0 0 0 0
       position (Light 0) $= Vertex4 (-20000) (-20000) 20000 0
       light (Light 1) $= Enabled
       ambient (Light 1) $= Color4 0 0 0 0
       diffuse (Light 1) $= Color4 0.15 0.15 0.15 0
       specular (Light 1) $= Color4 0 0 0 0
       position (Light 1) $= Vertex4 (20000) (-20000) 20000 0
       light (Light 2) $= Enabled
       ambient (Light 2) $= Color4 0 0 0 0
       diffuse (Light 2) $= Color4 0.15 0.15 0.15 0
       specular (Light 2) $= Color4 0 0 0 0
       position (Light 2) $= Vertex4 (-20000) (20000) 20000 0
       light (Light 3) $= Enabled
       ambient (Light 3) $= Color4 0 0 0 0
       diffuse (Light 3) $= Color4 0.75 0.75 0.75 0
       specular (Light 3) $= Color4 0 0 0 0
       position (Light 3) $= Vertex4 (20000) (20000) (-20000) 0
       blend $= Disabled
       fog $= Enabled
       fogColor $= (Color4 1 1 1 1)
       fogMode $= Linear 500 7500
       matrixMode $= Projection
       loadIdentity
       (Size width height) <- get windowSize
       perspective 45.0 ((fromInteger $ toInteger width)/(fromInteger $ toInteger height)) 10 10000
       matrixMode $= Modelview 0
       loadIdentity
       lookAt (Vertex3 0 0 0) (Vertex3 0 0 1) (Vector3 0 1 0)
       preservingMatrix $ renderModel
       light (Light 1) $= Disabled
       light (Light 2) $= Disabled
       light (Light 3) $= Disabled
       polygonSmooth $= Disabled
	   where renderModel = do cyc <- cycleSeconds 35
				  translate $ (Vector3 0 0 150 :: Vector3 Float)
				  rotate (360*cyc) (Vector3 1 1 1)				 
				  toOpenGL model

cycleSeconds :: Integer -> IO Float
cycleSeconds cycle_length = do (TOD secs picos) <- getClockTime
			       return $ (fromInteger (secs `mod` cycle_length) + (fromInteger picos / 1000000000000)) / fromInteger cycle_length

-- This isn't properly random, but it does create a nifty effect.
starfield :: [(Float,Float,Float)]
starfield = sortBy (\(x0,y0,z0) (x1,y1,z1) -> compare (x1*x1+y1*y1+z1*z1) (x0*x0+y0*y0*z0*z0)) $
    map genStar [1..1000]
	where genStar n = let (n',_) = next $ mkStdGen n
			      (x,g0) = next $ mkStdGen n'
			      (y,g1) = next g0
			      (z,_) = next g1
			      tform = fromInteger . toInteger
			      in (tform (x `mod` 1000) / 1000.0,
				  tform (y `mod` 1000) / 1000.0,
				  tform (z `mod` 1000) / 1000.0)

renderStars :: (Float,Float) -> (Float,Float) -> (Float,Float) -> [(Float,Float,Float)] -> IO ()
renderStars (minx,maxx) (miny,maxy) (minz,maxz) pts =
    do renderPrimitive Triangles $ mapM_ drawPoint pts
	   where drawPoint (x,y,z) = do color $ (Color4 1 1 1 1 :: Color4 Float)
				        (vertex $ Vertex3 
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz))
					color $ (Color4 0 0 1 0 :: Color4 Float)
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx-3)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz+500))
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx+3)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz+500))
					color $ (Color4 1 1 1 1 :: Color4 Float)
				        (vertex $ Vertex3 
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz))
					color $ (Color4 0 0 1 0 :: Color4 Float)
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny-6)
					 (z*(maxz-minz)+minz+250))
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny+6)
					 (z*(maxz-minz)+minz+250))
