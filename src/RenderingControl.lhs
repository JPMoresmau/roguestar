\section{The Top-Level Animation Loop}

\begin{code}
{-# LANGUAGE Arrows #-}

module RenderingControl
    (mainAnimationLoop)
    where

import Globals
import Data.List
import RSAGL.FRP
import RSAGL.Edge
import RSAGL.Vector
import Animation
import RSAGL.Angle
import Control.Arrow
import Data.Maybe
import PrintText
import Tables
import RSAGL.CoordinateSystems
import RSAGL.Affine
import RSAGL.ModelingExtras
import RSAGL.Time
import VisibleObject
import RSAGL.InverseKinematics
import Control.Applicative
import qualified Data.Map as Map
import RSAGL.LightSource
import Sky
import Scene
import Data.Monoid
import Starships
import AnimationTerrain
import AnimationCreatures
import AnimationTools
import AnimationMenus
import AnimationExtras
import AnimationEvents
\end{code}

\begin{code}
mainAnimationLoop :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
mainAnimationLoop = proc () ->
    do m_state <- driverGetAnswerA -< "state"
       switchContinue -< (fmap (const $ mainWelcome >>> mainDispatch) m_state,())
       printTextOnce -< Just (UnexpectedEvent,"Waiting for engine...")
       returnA -< roguestarSceneLayerInfo mempty basic_camera
  where mainWelcome = proc () ->
            do printTextOnce -< Just (Event,"Welcome to Roguestar-GL.")
	       returnA -< ()
\end{code}

\subsection{The Main Dispatch}

\texttt{mainDispatch} transfers control of the main thread to a
subprogram based on the state of the engine.  For example, menu-related
states dispatch to \texttt{menuDispatch} which manages the menu gui.

\texttt{mainHeader} guards against changes in the engine state.
Whenever the state changes, the header switches to \texttt{mainDispatch}.

\begin{code}
mainStateHeader :: (String -> Bool) -> RSAnimA1 () SceneLayerInfo () ()
mainStateHeader = genericStateHeader switchTo
  where switchTo menu_state | menu_state `elem` menu_states = menuManager
        switchTo planar_state | planar_state `elem` planar_states = planarGameplayDispatch
	switchTo "game-over" = gameOver
	switchTo unknown_state = error $ "mainStateHeader: unrecognized state: " ++ unknown_state

mainDispatch :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
mainDispatch = mainStateHeader (const False) >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)

menuManager :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuManager = proc () ->
    do mainStateHeader (`elem` menu_states) -< ()
       frp1Context menuDispatch -< ()
\end{code}

\subsection{The Game Over State}

\begin{code}
gameOver :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
gameOver = proc () ->
    do printTextOnce -< Just (Event,"You have been killed.")
       returnA -< roguestarSceneLayerInfo mempty basic_camera
\end{code}

\subsection{The Planar Gameplay Dispatch}

\begin{code}
planar_states :: [String]
planar_states = ["player-turn","turn","jump","attack","fire","attack-event","miss-event","killed-event","weapon-overheats-event","weapon-explodes-event","disarm-event","sunder-event"]

planarGameplayDispatch :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
planarGameplayDispatch = proc () ->
    do -- setup/get infos
       mainStateHeader (`elem` planar_states) -< () 
       clearPrintTextOnce -< ()
       frp1Context eventMessager -< ()
       -- terrain threads
       frpContext (maybeThreadIdentity terrainTileThreadIdentity) [(Nothing,terrainThreadLauncher)] -< ()
       -- creature threads
       ctos <- arr (catMaybes . map (uncurry $ liftA2 (,))) <<< 
           frpContext (maybeThreadIdentity $ unionThreadIdentity (==)) 
	       [(Nothing,visibleObjectThreadLauncher creatureAvatar)] -< ()
       -- tool threads
       frpContext (maybeThreadIdentity $ unionThreadIdentity (==)) [(Nothing,visibleObjectThreadLauncher toolAvatar)] -< 
           ToolThreadInput {
	       tti_wield_points = Map.fromList $ map (\(uid,cto) -> (uid,cto_wield_point cto)) ctos }
       -- cyborg planet killer, just there for no reason
       transformA cyborgType4 -< (Affine $ translate (Vector3D 3 4 (-3)) . scale' 0.1,StarshipInput)
       -- camera/lighting stuff, including sky sphere 
       sky_info <- getSkyInfo -< ()
       sky -< sky_info
       m_lookat <- whenJust (approachA 1.0 (perSecond 3.0)) <<< sticky isJust Nothing <<<
           arr (fmap (\(x,y) -> Point3D (realToFrac x) 0.25 (negate $ realToFrac y))) <<< centerCoordinates -< ()
       camera_distance <- approachA 5.0 (perSecond 5.0) <<< readGlobal global_planar_camera_distance -< ()
       let (planar_camera,lookat) = maybe (basic_camera,origin_point_3d) (\x -> (planarCamera camera_distance x,x)) m_lookat
       artificial_light_intensity <- arr lighting_artificial <<< lightingConfiguration -< sky_info
       accumulateSceneA -< (scene_layer_local, lightSource $ if artificial_light_intensity > 0.05
           then mapLightSource (mapBoth $ scaleRGB artificial_light_intensity) $ PointLight {
                    lightsource_position = camera_position planar_camera,
	            lightsource_radius = measure (camera_position planar_camera) lookat,
		    lightsource_color = gray 0.8,
		    lightsource_ambient = gray 0.2 }
	   else NoLight)
       returnA -< roguestarSceneLayerInfo (skyAbsorbtionFilter sky_info) planar_camera

planarCamera :: Double -> Point3D -> Camera
planarCamera camera_distance look_at = PerspectiveCamera {
    camera_position = translate (vectorScaleTo camera_distance $ Vector3D 0 (3*(camera_distance/3)**2) camera_distance) look_at,
    camera_lookat = translate (Vector3D 0 (1/camera_distance) 0) look_at,
    camera_up = Vector3D 0 1 0,
    camera_fov = fromDegrees $ 40 + 30 / camera_distance }

centerCoordinates :: RSAnimAX any t i o () (Maybe (Integer,Integer))
centerCoordinates = proc () ->
    do m_center_coordinates_table <- sticky isJust Nothing <<< driverGetTableA -< ("center-coordinates","0")
       returnA -< do center_coordinates_table <- m_center_coordinates_table
                     x <- tableLookupInteger center_coordinates_table ("axis","coordinate") "x" -- Maybe monad
                     y <- tableLookupInteger center_coordinates_table ("axis","coordinate") "y" 
		     return (x,y)
\end{code}

