\section{The Top-Level Animation Loop}

\begin{code}
{-# LANGUAGE Arrows #-}

module RenderingControl
    (mainAnimationLoop)
    where

import Data.List
import RSAGL.FRP
import RSAGL.Edge
import RSAGL.Scene
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
import RSAGL.Interpolation
import ProtocolTypes
import RSAGL.Time
import RSAGL.AbstractVector
import VisibleObject
import RSAGL.InverseKinematics
import RSAGL.AnimationExtras
import Actions
import Strings
import Control.Applicative
import qualified Data.Map as Map
import Limbs
import RSAGL.Joint
import RSAGL.AbstractVector
\end{code}

\begin{code}
mainAnimationLoop :: RSAnimA1 () Camera () Camera
mainAnimationLoop = proc () ->
    do m_state <- driverGetAnswerA -< "state"
       switchContinue -< (fmap (const $ mainWelcome >>> mainDispatch) m_state,())
       printTextOnce -< Just (UnexpectedEvent,"Waiting for engine...")
       returnA -< basic_camera
  where mainWelcome = proc () ->
            do printTextOnce -< Just (Event,"Welcome to Roguestar-GL.")
	       returnA -< ()

basic_camera :: Camera
basic_camera = PerspectiveCamera {
    camera_position = Point3D 0 0 0,
    camera_lookat = Point3D 0 0 1,
    camera_up = Vector3D 0 1 0,
    camera_fov = fromDegrees 60 }
\end{code}

\subsection{The Main Dispatch}

\texttt{mainDispatch} transfers control of the main thread to a
subprogram based on the state of the engine.  For example, menu-related
states dispatch to \texttt{menuDispatch} which manages the menu gui.

\texttt{mainHeader} guards against changes in the engine state.
Whenever the state changes, the header switches to \texttt{mainDispatch}.

\begin{code}
mainStateHeader :: (String -> Bool) -> RSAnimA1 () Camera () ()
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

mainDispatch :: RSAnimA1 () Camera () Camera
mainDispatch = mainStateHeader (const False) >>> arr (const basic_camera)
\end{code}

\subsection{The Menu Dispatch}

\begin{code}
menu_states :: [String]
menu_states = ["race-selection",
               "class-selection"]
menuManager :: RSAnimA1 () Camera () Camera
menuManager = proc () ->
    do mainStateHeader (`elem` menu_states) -< ()
       frp1Context menuDispatch -< ()

menuStateHeader :: (String -> Bool) -> RSAnimA1 () Camera () Camera
menuStateHeader f = genericStateHeader switchTo f >>> arr (const basic_camera)
  where switchTo "race-selection" = menuRaceSelection
        switchTo "class-selection" = menuClassSelection
        switchTo unknown_state = error $ "menuStateHeader: unrecognized state: " ++ unknown_state

menuDispatch :: RSAnimA1 () Camera () Camera
menuDispatch = menuStateHeader (const False) >>> arr (const basic_camera)

menuRaceSelection :: RSAnimA1 () Camera () Camera
menuRaceSelection = proc s -> 
    do menuStateHeader (== "race-selection") -< s
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< ()
       printMenuA select_race_action_names -< ()
       printTextA -< Just (Query,"Select a Race:")
       returnA -< basic_camera

menuClassSelection :: RSAnimA1 () Camera () Camera
menuClassSelection = proc () -> 
    do menuStateHeader (== "class-selection") -< ()
       changed <- edgep <<< sticky isJust Nothing <<<arr (fmap table_created) <<< driverGetTableA -< ("player-stats","0")
       switchContinue -< (if changed then Just menuClassSelection else Nothing,())
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< ()
       printCharacterStats 0 -< ()
       printMenuA select_base_class_action_names -< ()
       printMenuItemA "reroll" -< ()
       printTextA -< Just (Query,"Select a Class:")
       returnA -< basic_camera

printCharacterStats :: Integer -> RSAnimAX any t i o () ()
printCharacterStats unique_id = proc () ->
    do m_player_stats <- driverGetTableA -< ("player-stats",show unique_id)
       print1CharacterStat -< (m_player_stats,"str")
       print1CharacterStat -< (m_player_stats,"dex")
       print1CharacterStat -< (m_player_stats,"con")
       print1CharacterStat -< (m_player_stats,"int")
       print1CharacterStat -< (m_player_stats,"per")
       print1CharacterStat -< (m_player_stats,"cha")
       print1CharacterStat -< (m_player_stats,"mind")

print1CharacterStat :: RSAnimAX any t i o (Maybe RoguestarTable,String) ()
print1CharacterStat = proc (m_player_stats,stat_str) ->
    do let m_stat_int = (\x -> tableLookupInteger x ("property","value") stat_str) =<< m_player_stats
       printTextA -< fmap (\x -> (Event,hrstring stat_str ++ ": " ++ show x)) m_stat_int
\end{code}

\subsection{The Game Over State}

\begin{code}
gameOver :: RSAnimA1 () Camera () Camera
gameOver = proc () ->
    do printTextOnce -< Just (Event,"You have been killed.")
       returnA -< basic_camera
\end{code}

\subsection{The Planar Gameplay Dispatch}

\begin{code}
planar_states :: [String]
planar_states = ["player-turn","pickup","drop","wield","attack","miss","killed"]

planarGameplayDispatch :: RSAnimA1 () Camera () Camera
planarGameplayDispatch = proc () ->
    do mainStateHeader (`elem` planar_states) -< () 
       clearPrintTextOnce -< ()
       frp1Context eventMessager -< ()
       frpContext (maybeThreadIdentity terrainTileThreadIdentity) [(Nothing,terrainThreadLauncher)] -< ()
       ctos <- arr (catMaybes . map (uncurry $ liftA2 (,))) <<< 
           frpContext (maybeThreadIdentity $ unionThreadIdentity (==)) 
	       [(Nothing,visibleObjectThreadLauncher creatureAvatar)] -< ()
       frpContext (maybeThreadIdentity $ unionThreadIdentity (==)) [(Nothing,visibleObjectThreadLauncher toolAvatar)] -< 
           ToolThreadInput {
	       tti_wield_points = Map.fromList $ map (\(uid,cto) -> (uid,cto_wield_point cto)) ctos } 
       lookat <- whenJust (approachA 1.0 (perSecond 3.0)) <<< sticky isJust Nothing <<<
           arr (fmap (\(x,y) -> Point3D (realToFrac x) 0 (negate $ realToFrac y))) <<< centerCoordinates -< ()
       accumulateSceneA -< (Infinite,lightSource $ DirectionalLight (Vector3D 0.15 1 (-0.3)) (scaleRGB 0.4 $ rgb 1.0 0.9 0.75) (scaleRGB 0.2 $ rgb 0.75 0.9 1.0))
       returnA -< maybe basic_camera cameraLookAtToCamera lookat

cameraLookAtToCamera :: Point3D -> Camera
cameraLookAtToCamera look_at = PerspectiveCamera {
    camera_position = translate (Vector3D 0 3 3) look_at,
    camera_lookat = look_at,
    camera_up = Vector3D 0 1 0,
    camera_fov = fromDegrees 60 }

centerCoordinates :: RSAnimAX any t i o () (Maybe (Integer,Integer))
centerCoordinates = proc () ->
    do m_center_coordinates_table <- sticky isJust Nothing <<< driverGetTableA -< ("center-coordinates","0")
       returnA -< do center_coordinates_table <- m_center_coordinates_table
                     x <- tableLookupInteger center_coordinates_table ("axis","coordinate") "x" -- Maybe monad
                     y <- tableLookupInteger center_coordinates_table ("axis","coordinate") "y" 
		     return (x,y)
\end{code}

\subsection{Terrain}

\begin{code}
terrainThreadLauncher :: RSAnimA (Maybe ProtocolTypes.TerrainTile) () () () ()
terrainThreadLauncher = spawnThreads <<< arr (map (\x -> (Just x,terrainTile x))) <<< terrainElements

terrainTile :: ProtocolTypes.TerrainTile -> RSAnimA (Maybe ProtocolTypes.TerrainTile) () () () ()
terrainTile terrain_tile = proc () ->
    do t <- threadTime -< ()
       still_here <- renderTerrainTile terrain_tile -< fromSeconds $ min 0 $ toSeconds t - 1
       switchTerminate -< (if still_here then Nothing else Just $ terrainTile_Descending terrain_tile,())
       
terrainTile_Descending :: ProtocolTypes.TerrainTile -> RSAnimA (Maybe ProtocolTypes.TerrainTile) () () () ()
terrainTile_Descending terrain_tile = proc () ->
    do t <- threadTime -< ()
       killThreadIf -< t >= fromSeconds 1
       renderTerrainTile terrain_tile -< t
       returnA -< ()

renderTerrainTile :: ProtocolTypes.TerrainTile -> RSAnimA t i o Time Bool
renderTerrainTile (ProtocolTypes.TerrainTile terrain_type (x,y)) = proc t ->
    do let awayness = max 0 $ min 0.99 $ (toSeconds t)^2
       terrain_elements <- terrainElements -< ()
       transformA libraryA -< (Affine $ translate (Vector3D (realToFrac x) 0 (negate $ realToFrac y)) . scale' (1 - awayness),
                               (Local,Models.LibraryData.TerrainTile terrain_type))
       returnA -< isJust $ find (\a -> tt_xy a == (x,y)) terrain_elements

terrainElements :: RSAnimA t i o () [ProtocolTypes.TerrainTile]
terrainElements = arr (maybe [] tableSelectTyped) <<< sticky isJust Nothing <<< driverGetTableA <<< arr (const ("visible-terrain","0"))

terrainTileThreadIdentity :: ThreadIdentity ProtocolTypes.TerrainTile
terrainTileThreadIdentity = unionThreadIdentity (\a b -> tt_xy a == tt_xy b)
\end{code}

\subsection{Creatures and Objects}

\begin{code}
data CreatureThreadOutput = CreatureThreadOutput {
    cto_wield_point :: CoordinateSystem }

class AbstractEmpty a where
    abstractEmpty :: a

instance AbstractEmpty () where
    abstractEmpty = ()

instance AbstractEmpty (Maybe a) where
    abstractEmpty = Nothing

visibleObjectThreadLauncher :: (AbstractEmpty o) => RSAnimA (Maybe Integer) i o i o -> RSAnimA (Maybe Integer) i o i o
visibleObjectThreadLauncher avatarA = arr (const abstractEmpty) <<< spawnThreads <<< arr (map (\x -> (Just x,avatarA))) <<< allObjects <<< arr (const ())

objectTypeGuard :: (String -> Bool) -> RSAnimA (Maybe Integer) a b () ()
objectTypeGuard f = proc () ->
    do m_obj_type <- objectDetailsLookup "object-type" -< ()
       killThreadIf -< maybe False (not . f) m_obj_type

creatureAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
creatureAvatar = proc () ->
    do objectTypeGuard (== "creature") -< ()
       m_species <- objectDetailsLookup "species" -< ()
       switchContinue -< (fmap switchTo m_species,())
       returnA -< Nothing
  where switchTo "encephalon" = encephalonAvatar
        switchTo "recreant" = recreantAvatar
	switchTo "androsynth" = androsynthAvatar
	switchTo "ascendant" = ascendantAvatar
	switchTo "caduceator" = caduceatorAvatar
	switchTo "reptilian" = reptilianAvatar
        switchTo _ = questionMarkAvatar

genericCreatureAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () CreatureThreadOutput ->
                         RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
genericCreatureAvatar creatureA = proc () ->
    do visibleObjectHeader -< ()
       m_orientation <- objectIdealOrientation -< ()
       switchTerminate -< if isNothing m_orientation then (Just $ genericCreatureAvatar creatureA,Nothing) else (Nothing,Nothing)
       arr Just <<< transformA creatureA -< (fromMaybe (error "genericCreatureAvatar: fromMaybe") m_orientation,())

encephalonAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
encephalonAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (Local,Encephalon)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<< 
           bothArms MachineArmUpper MachineArmLower (Vector3D 0.66 0.66 0) (Point3D 0.145 0.145 0) 0.33 (Point3D 0.35 0.066 0.133) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

recreantAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
recreantAvatar = genericCreatureAvatar $ floatBobbing 0.25 0.4 $ proc () ->
    do libraryA -< (Local,Recreant)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms MachineArmUpper MachineArmLower (Vector3D 0 (-1.0) 0) (Point3D 0.3 0.075 0) 0.5 (Point3D 0.5 0.075 0.2) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

androsynthAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
androsynthAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (Local,Androsynth)
       bothLegs ThinLimb ThinLimb (Vector3D 0 0 1) (Point3D (0.07) 0.5 (-0.08)) 0.7 (Point3D 0.07 0 0.0) -< ()
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms ThinLimb ThinLimb (Vector3D (1.0) (-1.0) (-1.0)) (Point3D 0.05 0.65 0.0) 0.45 (Point3D 0.15 0.34 0.1) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

glower :: Point3D -> Vector3D -> RSAnimA (Maybe Integer) i o () ()
glower p_init v_init = proc () ->
    do local_origin <- exportToA root_coordinate_system -< origin_point_3d
       transformA
           (accelerationModel fps120 (p_init,perSecond $ v_init) 
                 (proc () -> 
	             do a <- derivative <<< derivative <<< exportToA root_coordinate_system -< origin_point_3d
	 	        returnA -< concatForces [quadraticTrap 10 p_init,
	                                         drag 1.0,
			    		         \_ _ _ -> scalarMultiply (-1) a,
						 \_ _ _ -> perSecond $ perSecond v_init,
						 \_ p _ -> perSecond $ perSecond $ vectorNormalize $
						               vectorToFrom origin_point_3d p `crossProduct` v_init]) 
	         (proc (_,()) -> libraryPointAtCamera -< (Local,AscendantGlow))) -< 
	             (translateToFrom local_origin origin_point_3d $ root_coordinate_system,())

ascendantAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
ascendantAvatar = genericCreatureAvatar $ proc () ->
    do glower (Point3D 0 0.5 0) zero -< ()
       glower (Point3D 0 0.5 0.35) (Vector3D 0 0 (-1)) -< ()
       glower (Point3D 0 0.5 (-0.35)) (Vector3D 0 0 1) -< ()
       glower (Point3D 0.35 0.5 0) (Vector3D (-1) 0 0) -< ()
       glower (Point3D (-0.35) 0.5 0) (Vector3D 1 0 0) -< ()
       accumulateSceneA -< (Local,
                            lightSource $ PointLight (Point3D 0 0.5 0)
                                                     (measure (Point3D 0 0.5 0) (Point3D 0 0 0))
						     azure
						     azure)
       t <- threadTime -< ()
       wield_point <- exportCoordinateSystem -< translate (rotateY (fromRotations $ t `cyclical'` (fromSeconds 3)) $ Vector3D 0.25 0.5 0)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

caduceatorAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
caduceatorAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (Local,Caduceator)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms CaduceatorArmUpper CaduceatorArmLower (Vector3D 1.0 (-1.0) 1.0) (Point3D 0.1 0.15 0.257) 0.34 (Point3D 0.02 0.17 0.4) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

reptilianAvatar :: RSAnimA (Maybe Integer) () (Maybe CreatureThreadOutput) () (Maybe CreatureThreadOutput)
reptilianAvatar = genericCreatureAvatar $ proc () ->
    do libraryA -< (Local,Reptilian)
       bothLegs ReptilianLegUpper ReptilianLegLower (Vector3D 0 0 1) (Point3D (0.05) 0.25 (-0.1)) 0.29 (Point3D 0.07 0 0.0) -< ()
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms ReptilianArmUpper ReptilianArmLower (Vector3D 1.0 0.0 1.0) (Point3D (0.05) 0.35 (-0.1)) 0.25 (Point3D 0.07 0.25 0.12) -< ()
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

toolAvatar :: RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
toolAvatar = proc tti ->
    do objectTypeGuard (== "tool") -< ()
       m_tool <- objectDetailsLookup "tool" -< ()
       switchContinue -< (fmap switchTo m_tool,tti)
       returnA -< ()
  where switchTo "phase_pistol" = phasePistolAvatar
        switchTo _ = questionMarkAvatar >>> arr (const ())

phasePistolAvatar :: RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
phasePistolAvatar = proc tti ->
    do visibleObjectHeader -< ()
       m_orientation <- wieldableObjectIdealOrientation -< tti
       transformA libraryA -< maybe (root_coordinate_system,(Local,NullModel))
                                    (\o -> (o,(Local,PhasePistol))) 
				    m_orientation

floatBobbing :: Double -> Double -> RSAnimAX any t i o j p -> RSAnimAX any t i o j p
floatBobbing ay by animationA = proc j ->
    do t <- threadTime -< ()
       let float_y = lerpBetween (-1,sine $ fromRotations $ t `cyclical'` (fromSeconds 5),1) (ay,by)
       transformA animationA -< (Affine $ translate (Vector3D 0 float_y 0),j)

questionMarkAvatar :: RSAnimA (Maybe Integer) i o i (Maybe CreatureThreadOutput)
questionMarkAvatar = proc _ ->
    do visibleObjectHeader -< ()
       t <- threadTime -< ()
       m_object_type <- objectDetailsLookup "object-type" -< ()
       m_species <- objectDetailsLookup "species" -< ()
       m_tool <- objectDetailsLookup "tool" -< ()                
       debugOnce -< if any (isJust) [m_object_type,m_species,m_tool] 
                    then (Just $ "questionMarkAvatar: apparently didn't recognize object: " ++ 
		                 show m_object_type ++ ", " ++ show m_species ++ ", " ++ show m_tool)
		    else Nothing
       m_position <- objectIdealPosition -< ()
       let float_y = sine $ fromRotations $ t `cyclical'` (fromSeconds 5)
       let m_transform = fmap (translate (Vector3D 0 (0.7 + float_y/10) 0)) m_position 
       transformA libraryA -< maybe (Affine id,(Local,NullModel)) (\p -> (Affine $ translateToFrom p origin_point_3d,(Local,QuestionMark))) m_transform
       m_wield_point <- whenJust exportCoordinateSystem -< fmap (\p -> translate (vectorToFrom p origin_point_3d `add` Vector3D 0.4 0 0)) m_transform 
       returnA -< 
           do wield_point <- m_wield_point
	      return $ CreatureThreadOutput {
                           cto_wield_point = wield_point }
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
    messageState "attack" $ proc () -> 
        do m_weapon <- driverGetAnswerA -< "weapon-used"
	   returnA -< 
	       do weapon <- m_weapon
	          return $ case () of
		      () | weapon == "0" -> "It attacks!  It hits!"
		      () | otherwise -> "You attack!  You hit!",
    messageState "miss" $ proc () -> 
        do m_weapon <- driverGetAnswerA -< "weapon-used"
	   returnA -<
	       do weapon <- m_weapon
	          return $ case () of
		      () | weapon == "0" -> "It attacks!  It misses."
		      () | otherwise -> "You attack!  You miss.",
    messageState "killed" $ proc () -> 
        do m_who_killed <- driverGetAnswerA -< "who-killed"
	   returnA -<
	       do who_killed <- m_who_killed
	          return $ case () of
		      () | who_killed == "2" -> "You are mortally wounded."
		      () | otherwise -> "You kill it!"]
\end{code}
