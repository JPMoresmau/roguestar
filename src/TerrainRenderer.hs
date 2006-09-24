module TerrainRenderer
    (resetTerrainRenderingFunction)
    where

import Data.Set as Set
import Data.List as List
import Driver
import Globals
import Data.IORef
import Tables
import Data.Maybe
import Control.Monad
import Graphics.Rendering.OpenGL.GL
import Seconds

terrainHeight :: String -> Float
terrainHeight "water" = 0.025
terrainHeight "deepwater" = 0.025
terrainHeight "sand" = 0.05
terrainHeight "desert" = 0.05
terrainHeight "grass" = 0.065
terrainHeight "dirt" = 0.06
terrainHeight "forest" = 0.075
terrainHeight "deepforest" = 0.085
terrainHeight "ice" = 0.025
terrainHeight "lava" = 0.03
terrainHeight "glass" = 0.02
terrainHeight "rockyground" = 0.1
terrainHeight "rubble" = 0.2
terrainHeight "rockface" = 0.6
terrainHeight _ = 0.0

terrainColor :: String -> Color4 Float
terrainColor "water" = Color4 0.5 0.5 1.0 1.0
terrainColor "deepwater" = Color4 0.5 0.5 1.0 1.0
terrainColor "sand" = Color4 1.0 0.9 0.3 1.0
terrainColor "desert" = Color4 1.0 0.9 0.3 1.0
terrainColor "grass" = Color4 0.1 0.9 0.1 1.0
terrainColor "dirt" = Color4 0.4 0.5 0.1 1.0
terrainColor "forest" = terrainColor "grass"
terrainColor "deepforest" = terrainColor "dirt"
terrainColor "ice" = Color4 0.9 1.0 1.0 1.0
terrainColor "lava" = Color4 1.0 0.0 0.0 1.0
terrainColor "glass" = Color4 0.0 0.0 0.0 1.0
terrainColor "rockyground" = Color4 0.5 0.5 0.5 1.0
terrainColor "rubble" = terrainColor "rockyground"
terrainColor "rockface" = terrainColor "rockyground"
terrainColor _ = Color4 1.0 0.0 1.0 1.0

terrainShininess :: String -> Float
terrainShininess "grass" = 0.1
terrainShininess "water" = 0.5
terrainShininess "deepwater" = 0.5
terrainShininess "ice" = 1.0
terrainShininess "glass" = 0.5
terrainShininess _ = 0.0

terrainGlows :: String -> Bool
terrainGlows "lava" = True
terrainGlows _ = False

-- |
-- The wavelength of this terrain's sinusoidal motion.
--
terrainCycle :: String -> Maybe Integer
terrainCycle "water" = Just 2
terrainCycle "deepwater" = Just 9
terrainCycle _ = Nothing

-- |
-- The amplitude of this terrain sinusoidal motion.
--
terrainLift :: String -> Float
terrainLift "water" = 0.002
terrainLift "deepwater" = 0.002
terrainLift _ = 0.0

--data TerrainSpecial = Trees Integer | RockFace

--terrainSpecial :: String -> Maybe TerrainSpecial
--terrainSpecial "forest" = Just $ Trees 1
--terrainSpecial "deepforest" = Just $ Trees 3
--terrainSpecial "rockface" = Just RockFace
--terrainSpecial _ = Nothing

-- |
-- Render a patch of terrain.  The first parameter is a set of all known terrain patches
-- the second parameter is the specific patch to render (coordinates,terrain type)
--
renderPatchAt :: ((Integer,Integer),String) -> IO ()
renderPatchAt ((x,y),patch_str) = 
    do cycle_seconds <- maybe (return 0) cycleSeconds $ terrainCycle patch_str
       let extra_lift = (if isNothing (terrainCycle patch_str) then 0
			 else terrainLift patch_str * (1 + sin (2*pi * cycle_seconds + fromInteger (x + y) / 20)))
	   extra_height = (if isJust (terrainCycle patch_str) then 0 
			   else terrainHeight patch_str * (cos $ fromInteger (x*y^2 `mod` 256)) * 0.5 + 0.01)
	   renderPatchTranslated = do translate $ (Vector3 (fromInteger x) extra_lift (fromInteger y) :: Vector3 Float)
				      renderPatch patch_str extra_height
       preservingMatrix renderPatchTranslated

renderPatch :: String -> Float -> IO ()
renderPatch patch_str extra_height =
    do materialShininess Front $= shininess*128
       materialSpecular Front $= Color4 shininess shininess shininess 1
       materialDiffuse Front $= the_color
       materialAmbient Front $= terrain_background_color
       materialEmission Front $= (if terrainGlows patch_str then the_color else Color4 0 0 0 1)
       plotPatch (the_height + extra_height)
	   where shininess = terrainShininess patch_str
		 the_color = terrainColor patch_str
		 the_height = terrainHeight patch_str

-- |
-- Plots a patch of terrain into OpenGL with the given height.
-- Only the shape, not color/texture etc.
--
plotPatch :: Float -> IO ()
plotPatch the_height = 
    renderPrimitive TriangleFan $ do normal $ (Normal3 0 1 0 :: Normal3 Float)
				     vertex $ (Vertex3 0 the_height 0 :: Vertex3 Float)
				     normal $ (Normal3 (-the_height) normal_y 0 :: Normal3 Float)
				     vertex $ (Vertex3 (-1) 0 0 :: Vertex3 Float)
				     normal $ (Normal3 0 normal_y the_height :: Normal3 Float)
				     vertex $ (Vertex3 0 0 1 :: Vertex3 Float)
				     normal $ (Normal3 the_height normal_y 0 :: Normal3 Float)
				     vertex $ (Vertex3 1 0 0 :: Vertex3 Float)
				     normal $ (Normal3 0 normal_y (-the_height) :: Normal3 Float)
				     vertex $ (Vertex3 0 0 (-1) :: Vertex3 Float)
				     normal $ (Normal3 (-the_height) normal_y 0 :: Normal3 Float)
				     vertex $ (Vertex3 (-1) 0 0 :: Vertex3 Float)
					 where normal_y = sqrt (1 - (the_height^2))

terrain_background_color :: Color4 Float
terrain_background_color = Color4 0.3 0.3 0.3 1.0

renderTerrain :: [((Integer,Integer),String)] -> IO ()
renderTerrain patches = 
    do mapM_ renderPatchAt patches
       materialShininess Front $= 0
       materialSpecular Front $= Color4 0 0 0 1.0
       materialAmbient Front $= terrain_background_color
       materialDiffuse Front $= Color4 0 0 0 1.0
       materialEmission Front $= Color4 0 0 0 1

-- |
-- Sets global_terrain_rendering_function according to the data in the 
-- "visible-terrain" table.  Returns True if the action was successful,
-- False otherwise (try again later).
--
resetTerrainRenderingFunction :: IORef RoguestarGlobals -> IO Bool
resetTerrainRenderingFunction globals_ref =
    do table <- driverRequestTable globals_ref "visible-terrain" "0" 
       when (isJust table) $
	    modifyIORef globals_ref ( \ globals -> globals { global_terrain_rendering_function = \_ ->
							     renderTerrain $ tableToTerrainData $ fromJust table } )
       return (isJust table)

tableToTerrainData :: RoguestarTable -> [((Integer,Integer),String)]
tableToTerrainData table = mapMaybe fromTable $ tableSelect3Integer table ("terrain-type","x","y")
    where fromTable (terrain_type,Just x,Just y) = Just ((x,y),terrain_type)
	  fromTable _ = Nothing