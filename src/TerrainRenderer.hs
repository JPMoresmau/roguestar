--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module TerrainRenderer
    (resetTerrainRenderingFunction,
     getTerrainHeight)
    where

import Data.Set as Set
import Data.List as List
import Data.Map as Map
import Driver
import Globals
import Data.IORef
import Tables
import Data.Maybe as Maybe
import Control.Monad
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.UI.GLUT.Objects as GLUT
import Time
import Math3D
import Model

data TerrainShape = TSPyramid | TSCone | TSCube deriving (Eq)

-- |
-- The depth information for most terrain patches is erased after 
-- the terrain has been drawn, and replaced with a simple flat plane.
-- This prevents the terrain from unrealistically obscuring creatures.
-- Some terrain, such as rock face, should obscure creatures.
-- For this terrain patches, we return true.
--
terrainIsDepthTestSignificant :: String -> Bool
terrainIsDepthTestSignificant str | terrainShape str == TSCube = True
terrainIsDepthTestSignificant _ = False

terrainShape :: String -> TerrainShape
terrainShape "rockface" = TSCube
terrainShape _ = TSCone

terrainHeight :: String -> Double
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
terrainHeight "rockyground" = 0.09
terrainHeight "rubble" = 0.1
terrainHeight "rockface" = 1.0
terrainHeight _ = 0.0

terrainColor :: String -> Color4 GLfloat
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

terrainShininess :: String -> GLfloat
terrainShininess "water" = 0.5
terrainShininess "deepwater" = 0.5
terrainShininess "ice" = 1.0
terrainShininess "glass" = 0.5
terrainShininess _ = 0.0

terrainGlows :: String -> Bool
terrainGlows "lava" = True
terrainGlows _ = False

-- |
-- The speed of this terrain's wave motion (smaller is faster).
--
terrainCycle :: String -> Maybe Integer
terrainCycle "water" = Just 2
terrainCycle "deepwater" = Just 3
terrainCycle _ = Nothing

-- |
-- The amplitude of this terrain's wave motion.
--
terrainLift :: String -> Double
terrainLift "water" = 0.002
terrainLift "deepwater" = 0.002
terrainLift _ = 0.0

terrainBreadth :: String -> Double
terrainBreadth patch_str =
    case terrainCycle patch_str of
                                Nothing -> 1.1
                                Just _ -> 1.25

-- |
-- How deep is the (liquid) terrain when you wade through it.
--
terrainDepth :: String -> Double
terrainDepth "water" = 0.05
terrainDepth "deepwater" = 0.1
terrainDepth "lava" = 0.05
terrainDepth _ = 0.0

--data TerrainSpecial = Trees Integer

--terrainSpecial :: String -> Maybe TerrainSpecial
--terrainSpecial "forest" = Just $ Trees 1
--terrainSpecial "deepforest" = Just $ Trees 3
--terrainSpecial _ = Nothing

-- |
-- The height of the center of a patch of terrain, given the coordinates
-- and type of terrain.
--
terrainHeightAt :: ((Integer,Integer),String) -> Double
terrainHeightAt ((x,y),patch_str) = 
    if isJust (terrainCycle patch_str) -- don't apply height variation to moving tiles, it doesn't look good 
    then base_height
    else base_height + base_height * (cos $ fromInteger (x*y^2 `mod` 256)) * 0.25 + 0.01
       where base_height = terrainHeight patch_str

getTerrainHeight :: IORef RoguestarGlobals -> (Integer,Integer) -> IO Double
getTerrainHeight globals_ref pt = 
    do patch_str <- liftM (fromMaybe "unknown_terrain" . Map.lookup pt . global_terrain_data) $ readIORef globals_ref
       lift <- terrainLiftAt (pt,patch_str)
       return $ if terrainIsDepthTestSignificant patch_str
                then terrainHeightAt (pt,patch_str)
                else - lift - (terrainDepth patch_str)
        

-- |
-- Get the sinusoidal lift of a patch of terrain (zero for anythinge except water, lava, etc)
--
terrainLiftAt :: ((Integer,Integer),String) -> IO Double
terrainLiftAt ((x,y),patch_str) =     
    case terrainCycle patch_str of 
        Nothing -> return 0
        Just terrain_cycle -> do cycle_seconds <- cycleSeconds $ terrain_cycle
                                 return $ terrainLift patch_str * (1 + sin (2*pi * cycle_seconds + fromInteger (x + y) / (fromInteger terrain_cycle)))
    

-- |
-- Render a patch of terrain given the coordinates and type of terrain.
--
renderPatchAt :: ((Integer,Integer),String) -> IO ()
renderPatchAt patch_data@((x,y),patch_str) = preservingMatrix $
    do extra_lift <- terrainLiftAt patch_data
       GL.translate $ (Vector3 (fromInteger x) extra_lift (fromInteger y) :: Vector3 Double)
       materialShininess Front $= shininess*128
       materialSpecular Front $= Color4 shininess shininess shininess 1
       materialDiffuse Front $= the_color
       materialAmbient Front $= terrain_background_color
       materialEmission Front $= (if terrainGlows patch_str then the_color else Color4 0 0 0 1)
       plotPatch (terrainShape patch_str) (terrainHeightAt patch_data) (terrainBreadth patch_str)
	   where shininess = terrainShininess patch_str
		 the_color = terrainColor patch_str

-- |
-- Plots a pyramid-shaped patch of terrain into OpenGL with the given height and breadth. 
--
plotPatch :: TerrainShape -> Double -> Double -> IO ()
plotPatch TSPyramid = plotPatch_PyramidIncrements 4
plotPatch TSCone = plotPatch_PyramidIncrements 8
plotPatch TSCube = plotPatch_Cube

plotPatch_PyramidIncrements :: Integer -> Double -> Double -> IO ()
plotPatch_PyramidIncrements increments the_height the_breadth = 
    renderPrimitive TriangleFan $ 
        do plotPatch_Apex the_height
           mapM_ (plotPatch_Vertex the_height the_breadth) $ zero_angle : (reverse $ radianIncrements increments)

plotPatch_Cube :: Double -> Double -> IO ()
plotPatch_Cube the_height the_breadth =
    Math3D.scale (Vector3D the_breadth the_height the_breadth) $ renderObject Solid (Cube 1.0)

plotPatch_Apex :: Double -> IO ()
plotPatch_Apex the_height = 
    do normal $ (Normal3 0 1 0 :: Normal3 Double)
       vertex $ (Vertex3 0 the_height 0 :: Vertex3 Double)

plotPatch_Vertex :: Double -> Double -> Angle -> IO ()
plotPatch_Vertex the_height the_breadth rotation = 
    do normal $ (Normal3 (cosr * the_height) normal_y (sinr * the_height) :: Normal3 Double)
       vertex $ (Vertex3 (cosr * the_breadth) 0 (sinr * the_breadth) :: Vertex3 Double)
           where normal_y = sqrt (1 - ((the_height/the_breadth)^2))
                 cosr = cosine rotation
                 sinr = sine rotation

terrain_background_color :: Color4 GLfloat
terrain_background_color = Color4 0.3 0.3 0.3 1.0

renderTerrain :: [((Integer,Integer),String)] -> Bool -> IO ()
renderTerrain patches color_buffer_enabled = 
    do mapM_ renderPatchAt $ List.filter ((color_buffer_enabled ||) . terrainIsDepthTestSignificant . snd) patches
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
           do let terrain_data = tableToTerrainData $ fromJust table
	      modifyIORef globals_ref ( \ globals -> globals { global_terrain_rendering_function = \_ -> renderTerrain terrain_data,  
	                                                       global_terrain_data = Map.fromList terrain_data } )
       return (isJust table)

tableToTerrainData :: RoguestarTable -> [((Integer,Integer),String)]
tableToTerrainData table = Maybe.mapMaybe fromTable $ tableSelectFormatted table [TDString "terrain-type",TDNumber "x",TDNumber "y"]
    where fromTable [TDString terrain_type,TDNumber x,TDNumber y] = Just ((x,y),terrain_type)
          fromTable _ = Nothing