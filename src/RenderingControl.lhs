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
import Models.LibraryData
import RSAGL.CoordinateSystems
import RSAGL.Affine
import RSAGL.ModelingExtras
import RSAGL.Time
import VisibleObject
import RSAGL.InverseKinematics
import Actions
import Strings
import Control.Applicative
import qualified Data.Map as Map
import RSAGL.LightSource
import Sky
import Scene
import Data.Monoid
import Starships
import AnimationTerrain
import AnimationCreatures
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

basic_camera :: Camera
basic_camera = PerspectiveCamera {
    camera_position = Point3D 0 0 0,
    camera_lookat = Point3D 0 0 1,
    camera_up = Vector3D 0 1 0,
    camera_fov = fromDegrees 45 }
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

genericStateHeader :: (String -> RSAnimA1 i o i o) -> (String -> Bool) -> RSAnimA1 i o i ()
genericStateHeader switchTo f = proc i ->
    do m_state <- driverGetAnswerA -< "state"
       switchContinue -< (if fmap f m_state == Just True then Nothing else fmap switchTo m_state,i)
       returnA -< ()

mainDispatch :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
mainDispatch = mainStateHeader (const False) >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)
\end{code}

\subsection{The Menu Dispatch}

\begin{code}
menu_states :: [String]
menu_states = ["race-selection",
               "class-selection",
               "pickup",
               "drop",
               "wield"]
menuManager :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuManager = proc () ->
    do mainStateHeader (`elem` menu_states) -< ()
       frp1Context menuDispatch -< ()

menuStateHeader :: (String -> Bool) -> RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuStateHeader f = genericStateHeader switchTo f >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)
  where switchTo "race-selection" = menuRaceSelection
        switchTo "class-selection" = menuClassSelection
        switchTo "pickup" = toolMenuSelection
        switchTo "drop" = toolMenuSelection
        switchTo "wield" = toolMenuSelection
        switchTo unknown_state = error $ "menuStateHeader: unrecognized state: " ++ unknown_state

menuDispatch :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuDispatch = menuStateHeader (const False) >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)

menuRaceSelection :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuRaceSelection = proc s -> 
    do menuStateHeader (== "race-selection") -< s
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< ()
       printMenuA select_race_action_names -< ()
       printTextA -< Just (Query,"Select a Race:")
       returnA -< roguestarSceneLayerInfo mempty basic_camera

menuClassSelection :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuClassSelection = proc () -> 
    do menuStateHeader (== "class-selection") -< ()
       changed <- edgep <<< sticky isJust Nothing <<< arr (fmap table_created) <<< driverGetTableA -< ("player-stats","0")
       switchContinue -< (if changed then Just menuClassSelection else Nothing,())
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< ()
       printCharacterStats 0 -< ()
       printMenuA select_base_class_action_names -< ()
       printMenuItemA "reroll" -< ()
       printTextA -< Just (Query,"Select a Class:")
       returnA -< roguestarSceneLayerInfo mempty basic_camera

printCharacterStats :: Integer -> RSAnimAX any t i o () ()
printCharacterStats unique_id = proc () ->
    do m_player_stats <- driverGetTableA -< ("player-stats",show unique_id)
       print1CharacterStat -< (m_player_stats,"str")
       print1CharacterStat -< (m_player_stats,"spd")
       print1CharacterStat -< (m_player_stats,"con")
       print1CharacterStat -< (m_player_stats,"int")
       print1CharacterStat -< (m_player_stats,"per")
       print1CharacterStat -< (m_player_stats,"cha")
       print1CharacterStat -< (m_player_stats,"mind")

print1CharacterStat :: RSAnimAX any t i o (Maybe RoguestarTable,String) ()
print1CharacterStat = proc (m_player_stats,stat_str) ->
    do let m_stat_int = (\x -> tableLookupInteger x ("property","value") stat_str) =<< m_player_stats
       printTextA -< fmap (\x -> (Event,hrstring stat_str ++ ": " ++ show x)) m_stat_int

toolMenuSelection :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
toolMenuSelection = proc () ->
    do menuStateHeader (`elem` ["pickup","drop","wield"]) -< ()
       state <- sticky isJust Nothing <<< driverGetAnswerA -< "menu-state"
       m_menu_data <- sticky isJust Nothing <<< driverGetTableA -< ("menu","7")
       menu_state <- sticky isJust Nothing <<< driverGetAnswerA -< "menu-state"
       clearPrintTextA -< ()
       requestPrintTextMode -< Unlimited
       printTextA -< Just (Query, unlines $ flip (maybe []) m_menu_data $ \menu_data -> flip map (tableSelect menu_data ["n","name"]) $ \[n,name] ->
           case Just n == menu_state of
               True ->  " ---> " ++ hrstring name
               False -> "      " ++ hrstring name)
       printTextA -< Just (Query, case state of
           Just "pickup" -> "Select an item to pick up: "
           Just "drop" -> "Select an item to drop: "
           Just "wield" -> "Select an item to wield: "
           _ -> "Select an item: ")
       printMenuItemA "next" -< ()
       printMenuItemA "prev" -< ()
       printMenuItemA "escape" -< ()
       returnA -< roguestarSceneLayerInfo mempty basic_camera
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

\subsection{Creatures and Objects}

\begin{code}
toolAvatar :: RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
toolAvatar = proc tti ->
    do objectTypeGuard (== "tool") -< ()
       m_tool <- objectDetailsLookup "tool" -< ()
       switchContinue -< (fmap switchTo m_tool,tti)
       returnA -< ()
  where switchTo "phase_pistol" = phaseWeaponAvatar PhasePistol
        switchTo "phaser" = phaseWeaponAvatar Phaser
        switchTo "phase_rifle" = phaseWeaponAvatar PhaseRifle
        switchTo "kinetic_fleuret" = energySwordAvatar Yellow 2
        switchTo "kinetic_sabre" = energySwordAvatar Yellow 4
        switchTo _ = questionMarkAvatar >>> arr (const ())

phaseWeaponAvatar :: LibraryModel -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
phaseWeaponAvatar phase_weapon_model = proc tti ->
    do visibleObjectHeader -< ()
       m_orientation <- wieldableObjectIdealOrientation -< tti
       whenJust (transformA libraryA) -< fmap (\o -> (o,(scene_layer_local,phase_weapon_model))) m_orientation
       returnA -< ()

energySwordAvatar :: EnergyColor -> Integer -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
energySwordAvatar energy_color sword_size = proc tti ->
    do visibleObjectHeader -< ()
       m_orientation <- wieldableObjectIdealOrientation -< tti
       is_being_wielded <- isBeingWielded -< ()
       whenJust (transformA displayA) -< fmap (\o -> (o,is_being_wielded)) m_orientation
       returnA -< ()
  where displayA :: RSAnimA1 i o Bool ()
        displayA = scale' (1/75) $ proc is_being_wielded ->
            do blade_length <- approachFrom 1 (perSecond 65) 0 -< if is_being_wielded then 10 * realToFrac sword_size else 0
               libraryA -< (scene_layer_local,EnergySword energy_color sword_size)
               transformA libraryA -< (Affine $ translate (Vector3D 0 2.9 0) . scale (Vector3D 1 blade_length 1),(scene_layer_local,EnergyCylinder energy_color))
\end{code}

\subsection{Messages}

\begin{code}
eventStateHeader :: (String -> Bool) -> RSAnimA1 () () () ()
eventStateHeader = genericStateHeader switchTo
    where switchTo s = fromMaybe eventMessager $ lookup s messages

eventMessager :: RSAnimA1 () () () ()
eventMessager = proc () -> 
    do eventStateHeader (isNothing . flip lookup messages) -< () 
       blockContinue -< True 

messageState :: String -> RSAnimA1 () () () (Maybe String) -> (String,RSAnimA1 () () () ())
messageState s actionA = (s,eventStateHeader (== s) >>> (proc () ->
    do m_string <- actionA -< ()
       blockContinue -< isNothing m_string
       printTextOnce -< fmap ((,) Event) m_string))

messages :: [(String,RSAnimA1 () () () ())]
messages = [
    messageState "attack-event" $ proc () -> 
        do m_weapon <- driverGetAnswerA -< "weapon-used"
	   returnA -< 
	       do weapon <- m_weapon
	          return $ case () of
		      () | weapon == "0" -> "It attacks!\nIt hits!"
		      () | otherwise -> "You attack!\nYou hit!",
    messageState "miss-event" $ proc () -> 
        do m_weapon <- driverGetAnswerA -< "weapon-used"
	   returnA -<
	       do weapon <- m_weapon
	          return $ case () of
		      () | weapon == "0" -> "It attacks!\nIt misses."
		      () | otherwise -> "You attack!\nYou miss.",
    messageState "killed-event" $ proc () -> 
        do m_who_killed <- driverGetAnswerA -< "who-killed"
	   returnA -<
	       do who_killed <- m_who_killed
	          return $ case () of
		      () | who_killed == "2" -> "You are mortally wounded."
		      () | otherwise -> "You kill it!",
    messageState "weapon-overheats-event" $ proc () ->
       do m_who_surprised <- driverGetAnswerA -< "who-attacks"
          returnA -<
              do who_surprised <- m_who_surprised
                 return $ case () of
                     () | who_surprised == "2" -> "You attack!\nOuch!  Your weapon overheats!"
                     () | otherwise -> "It attacks!\nIt's weapon overheats!",
    messageState "weapon-explodes-event" $ proc () ->
        do m_who_surprised <- driverGetAnswerA -< "who-attacks"
           returnA -<
               do who_surprised <- m_who_surprised
                  return $ case () of
                      () | who_surprised == "2" -> "You attack!\nYour weapon explodes in your hand!\nAre you sure you're even qualified to operate a directed energy firearm?"
                      () | otherwise -> "It attacks!\nIts weapon explodes in its hands!",
    messageState "disarm-event" $ proc () ->
        do m_who_attacks <- driverGetAnswerA -< "who-attacks"
           returnA -<
               do who_attacks <- m_who_attacks
                  return $ case () of
                      () | who_attacks == "2" -> "You attack!\nYou disarm it!"
                      () | otherwise -> "It attacks!\nIt disarms you!",
    messageState "sunder-event" $ proc () ->
        do m_who_attacks <- driverGetAnswerA -< "who-attacks"
           returnA -<
               do who_attacks <- m_who_attacks
                  return $ case () of
                      () | who_attacks == "2" -> "You attack!\nYou sunder it's weapon!"
                      () | otherwise -> "It attacks!\nIt sunders your weapon!"]
\end{code}
