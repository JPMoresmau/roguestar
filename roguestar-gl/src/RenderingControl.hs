{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies #-}

module RenderingControl
    (mainAnimationLoop)
    where

import Globals
import RSAGL.FRP
import RSAGL.Math
import RSAGL.Modeling
import RSAGL.Color
import Animation
import Control.Arrow
import Data.Maybe
import PrintText
import Tables
import VisibleObject
import RSAGL.Animation
import qualified Data.Map as Map
import Sky
import Scene
import Data.Monoid
import AnimationTerrain
import AnimationCreatures
import AnimationBuildings
import AnimationTools
import AnimationMenus
import AnimationExtras
import AnimationEvents
import Strings
import RSAGL.Math.Types
import PrintTextData
import qualified Data.ByteString.Char8 as B

-- | Enters the top-level animation loop.
mainAnimationLoop :: FRP e (FRP1 AnimationState () SceneLayerInfo) () SceneLayerInfo
mainAnimationLoop = proc () ->
    do m_state <- driverGetAnswerA -< "state"
       switchContinue -< (fmap (const $ mainWelcome >>> mainDispatch) m_state,())
       printTextOnce -< Just (UnexpectedEvent,"Waiting for engine...")
       returnA -< roguestarSceneLayerInfo mempty basic_camera
  where mainWelcome = proc () ->
            do printTextOnce -< Just (Event,"Welcome to Roguestar-GL.")
               returnA -< ()

-- | This is the actual to-level animation loop.
mainDispatch :: (FRPModel m) => FRP e (RSwitch Disabled () () SceneLayerInfo m) () SceneLayerInfo
mainDispatch = proc () ->
    do result <- frp1Context (mainStateHeader (const False) >>>
           arr (const $ roguestarSceneLayerInfo mempty basic_camera)) -< ()
       m_planet_name <- sticky isJust Nothing <<< driverGetAnswerA -< "planet-name"
       statusA -< fmap ((,) PlanetName) $ case m_planet_name of
           Just "nothing" -> Nothing
           x -> x
       m_dungeon_depth <- sticky isJust Nothing <<< driverGetAnswerA -< "dungeon-depth"
       statusA -< fmap ((,) DungeonDepth) m_dungeon_depth
       m_compass <- sticky isJust Nothing <<< driverGetAnswerA -< "compass"
       statusA -< fmap ((,) CompassHeading) $ case m_compass of
           Just "nothing" -> Nothing
           Just "here" -> Nothing
           x -> x
       returnA -< result

-- | Forces the current RSAnim thread to switch whenever the current state does not match the specified predicate.
-- Any switch that wants to surrender control whenever the state changes should call this first.
mainStateHeader :: (FRPModel m) => (B.ByteString -> Bool) -> FRP e (RSwitch Disabled () () SceneLayerInfo m) () ()
mainStateHeader = genericStateHeader switchTo
  where switchTo blanking_state | blanking_state `elem` blanking_states = blankingDispatch blanking_state
        switchTo menu_state | menu_state `elem` menu_states = menuManager
        switchTo planar_state | planar_state `elem` planar_states = planarGameplayDispatch
	switchTo "game-over" = gameOver
	switchTo unknown_state = error $ "mainStateHeader: unrecognized state: " ++ B.unpack unknown_state

-- | Displays all menus with a black background.
menuManager :: (FRPModel m) => FRP e (RSwitch Disabled () () SceneLayerInfo m) () SceneLayerInfo
menuManager = proc () ->
    do mainStateHeader (`elem` menu_states) -< ()
       frp1Context menuDispatch -< ()

-- | Print the game over message and retain control of the rendering loop forever.
gameOver :: (FRPModel m) => FRP e (RSwitch Disabled () () SceneLayerInfo m) () SceneLayerInfo
gameOver = proc () ->
    do printTextOnce -< Just (Event,"Game Over")
       returnA -< roguestarSceneLayerInfo mempty basic_camera

-- | List of all states that require the display of the planar environment (terrain, characters, tools, and sky)
planar_states :: [B.ByteString]
planar_states = ["player-turn","move","turn","jump","attack","fire","clear-terrain"] ++ recognized_events

-- | Captures all planar visuals: terrain, characters, tools, and sky.
planarGameplayDispatch :: (FRPModel m) => FRP e (RSwitch Disabled () () SceneLayerInfo m) () SceneLayerInfo
planarGameplayDispatch = proc () ->
    do -- setup/get infos
       mainStateHeader (`elem` planar_states) -< () 
       clearPrintTextOnce -< ()
       frp1Context eventMessager -< ()
       -- terrain threads
       frpContext (allowAnonymous forbidDuplicates) [(Nothing,terrainThreadLauncher)] -< ()
       -- building threads
       frpContext (allowAnonymous forbidDuplicates) [(Nothing,visibleObjectThreadLauncher buildingAvatar)] -< ()
       -- creature threads
       ctos <- arr (catMaybes . map (uncurry $ liftA2 (,))) <<< 
           frpContext (allowAnonymous forbidDuplicates) 
              [(Nothing,visibleObjectThreadLauncher creatureAvatar)] -< ()
       -- tool threads
       frpContext (allowAnonymous forbidDuplicates) [(Nothing,visibleObjectThreadLauncher toolAvatar)] -< 
           ToolThreadInput {
               tti_wield_points = Map.fromList $ map (\(uid,cto) -> (uid,cto_wield_point cto)) ctos }
       -- camera/lighting stuff, including sky sphere 
       sky_info <- getSkyInfo -< ()
       sky -< sky_info
       m_lookat <- whenJust (approachA 1.0 (perSecond 3.0)) <<< sticky isJust Nothing <<<
           arr (fmap (\(x,y) -> Point3D (realToFrac x) 0.25 (negate $ realToFrac y))) <<< centerCoordinates -< ()
       camera_distance <- approachA 5.0 (perSecond 5.0) <<< readGlobal global_planar_camera_distance -< ()
       let (planar_camera,lookat) = maybe (basic_camera,origin_point_3d) (\x -> (planarCamera camera_distance x,x)) m_lookat
       artificial_light_intensity <- arr lighting_artificial <<< lightingConfiguration -< sky_info
       sky_on <- readGlobal global_sky_on -< ()
       accumulateSceneA -< (scene_layer_local, lightSource $ case () of
           () | artificial_light_intensity > 0.05 && sky_on ->
                    mapLightSource (mapBoth $ scalarMultiply artificial_light_intensity) $ PointLight {
                        lightsource_position = camera_position planar_camera,
                        lightsource_radius = measure (camera_position planar_camera) lookat,
                        lightsource_color = grayscale 0.8,
                        lightsource_ambient = grayscale 0.2 }
           () | artificial_light_intensity > 0.05 -> PointLight {
                        lightsource_position = camera_position planar_camera,
                        lightsource_radius = measure (camera_position planar_camera) lookat,
                        lightsource_color = grayscale 0.5,
                        lightsource_ambient = grayscale 0.5 }
           () | otherwise -> NoLight)
       returnA -< roguestarSceneLayerInfo (skyAbsorbtionFilter sky_info) planar_camera

-- | Sets up a Camera, based on a camera distance parameter (probably tied to the global_planar_camera_distance variable)
-- and the look-at point.
planarCamera :: RSdouble -> Point3D -> Camera
planarCamera camera_distance look_at = PerspectiveCamera {
    camera_position = translate (vectorScaleTo camera_distance $ Vector3D 0 (7*(camera_distance/10)**2) camera_distance) look_at,
    camera_lookat = translate (Vector3D 0 (1/camera_distance) 0) look_at,
    camera_up = Vector3D 0 1 0,
    camera_fov = fromDegrees 75 }

-- | Retrieve the look-at point from the engine.
centerCoordinates :: (FRPModel m, FRPModes m ~ RoguestarModes) =>
                     FRP e m () (Maybe (Integer,Integer))
centerCoordinates = proc () ->
    do m_center_coordinates_table <- sticky isJust Nothing <<< driverGetTableA -< ("center-coordinates","0")
       returnA -< do center_coordinates_table <- m_center_coordinates_table
                     x <- tableLookupInteger center_coordinates_table ("axis","coordinate") "x" -- Maybe monad
                     y <- tableLookupInteger center_coordinates_table ("axis","coordinate") "y" 
		     return (x,y)

-- | A list of the states that require the screen to be blanked (made black), interrupting the planar visuals.
blanking_states :: [B.ByteString]
blanking_states = ["teleport-event","climb-event"]

-- | Display the blanked screen and print any blanking events.
blankingDispatch :: (FRPModel m) => B.ByteString -> FRP e (RSwitch Disabled () () SceneLayerInfo m) () SceneLayerInfo
blankingDispatch "teleport-event" = proc () ->
    do mainStateHeader (== "teleport-event") -< ()
       clearPrintTextOnce -< ()
       printTextOnce -< Just (Event,"Whoosh!")
       blockContinue <<< arr ((< 0.5) . toSeconds) <<< threadTime -< ()
       returnA -< roguestarSceneLayerInfo mempty basic_camera
blankingDispatch "climb-event" = proc () ->
    do mainStateHeader (== "climb-event") -< ()
       clearPrintTextOnce -< ()
       printTextOnce -< Just (Event,"You climb through a network of underground tunnels . . .")
       blockContinue <<< arr ((< 0.5) . toSeconds) <<< threadTime -< ()
       returnA -< roguestarSceneLayerInfo mempty basic_camera
blankingDispatch blanking_state = proc () ->
    do debugOnce -< Just $ "blankingDispatch: unrecognized blanking_state `" `B.append` blanking_state `B.append` "`"
       returnA -< roguestarSceneLayerInfo mempty basic_camera

