{-# OPTIONS_GHC -fglasgow-exts #-}

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

module ObjectRenderer 
  (renderObjects)
  where

import Data.Maybe
import Control.Monad
import Graphics.Rendering.OpenGL.GL as GL
import Quality
import Tables
import Globals
import Data.IORef
import Driver
import Math3D
import Joint
import Model
import Models.Encephalon
import Models.LibraryData
import Models.Library
import System.IO
import TerrainRenderer
import Time
import qualified Data.Map as Map
import Camera
import Animation

data ObjectRepresentation = ObjectRepresentation { object_rep_uid :: String,
                                                   object_rep_model :: String,
                                                   object_rep_position :: (Double,Double),
                                                   object_rep_altitude :: Double,
                                                   object_rep_heading_degrees :: Angle }

data ObjectAnimationStop = ObjectAnimationStop { object_anim_stop_object_rep :: ObjectRepresentation,
                                                 object_anim_stop_uptodate :: Time }

data ObjectAnimation = ObjectAnimation { object_anim_old :: ObjectRepresentation,
                                         object_anim_new :: ObjectRepresentation,
                                         object_anim_starts :: Time }

instance Lerpable ObjectRepresentation where
    lerp _ (a,b) | object_rep_uid a /= object_rep_uid b = error "ObjectRepresentation; lerp: tried to lerp different objects"
    lerp u (a,b) = ObjectRepresentation { object_rep_uid = object_rep_uid a,
                                          object_rep_model = object_rep_model a,
                                          object_rep_position = lerp u (object_rep_position a,object_rep_position b),
                                          object_rep_altitude = lerp u (object_rep_altitude a,object_rep_altitude b),
                                          object_rep_heading_degrees = lerp u (object_rep_heading_degrees a,object_rep_heading_degrees b) }

getObjectAnimations :: IORef RoguestarGlobals -> IO [ObjectAnimation]
getObjectAnimations globals_ref =
    do new_objects <- liftM format $ driverGetTable globals_ref Anything "visible-objects" "0"
       old_objects <- liftM format $ driverGetTable globals_ref Old "visible-objects" "0"
       new_object_anim_stops <- liftM catMaybes $ mapM (positionInfoToObjectAnimStop globals_ref New) new_objects
       old_object_anim_stops <- liftM catMaybes $ mapM (positionInfoToObjectAnimStop globals_ref Old) old_objects
       return $ mergeObjectAnimations new_object_anim_stops old_object_anim_stops
         where format_tableSelect = flip tableSelectFormatted [TDString "object-unique-id",TDNumber "x",TDNumber "y",TDString "facing"]
               format_toTuple = \e -> case e of
                                             [TDString uid,TDNumber x,TDNumber y,TDString f] -> [(uid,x,y,f)]
                                             _ -> []
               format = maybe [] (concatMap format_toTuple . format_tableSelect)

animateObject :: Time -> ObjectAnimation -> ObjectRepresentation
animateObject seconds_now anim = lerp ((min 1.0 $ toSeconds $ seconds_now - object_anim_starts anim) :: Double) 
                                      (object_anim_old anim,object_anim_new anim)

positionInfoToObjectAnimStop :: IORef RoguestarGlobals -> DataFreshness -> (String,Integer,Integer,String) -> IO (Maybe ObjectAnimationStop)
positionInfoToObjectAnimStop globals_ref freshness (uid,x,y,facing) =
    do object_details <- driverGetTable globals_ref freshness "object-details" uid
       altitude <- getTerrainHeight globals_ref (x,y)
       return $ (Just . objectAnimationStop (uid,x,y,facing) altitude) =<< object_details

-- |
-- Merge new and old animation stops to make an animation.
--
mergeObjectAnimations :: [ObjectAnimationStop] -> [ObjectAnimationStop] -> [ObjectAnimation]
mergeObjectAnimations new_anim_stops old_anim_stops =
    let objectAnimStopMap = Map.fromList . map (\x -> (object_rep_uid $ object_anim_stop_object_rep x,x))
        old_anim_stops_map = objectAnimStopMap old_anim_stops
        new_anim_stops_map = objectAnimStopMap new_anim_stops
        merge new old = ObjectAnimation { object_anim_old = object_anim_stop_object_rep old,
                                          object_anim_new = object_anim_stop_object_rep new,
                                          object_anim_starts = object_anim_stop_uptodate new }
        start new = ObjectAnimation { object_anim_old = object_anim_stop_object_rep new,
                                      object_anim_new = object_anim_stop_object_rep new,
                                      object_anim_starts = object_anim_stop_uptodate new }
        stop old = ObjectAnimation { object_anim_old = object_anim_stop_object_rep old,
                                     object_anim_new = object_anim_stop_object_rep old,
                                     object_anim_starts = object_anim_stop_uptodate old }
        merges = Map.toList $ Map.intersectionWith merge new_anim_stops_map old_anim_stops_map
        news = Map.toList $ Map.map start $ Map.difference new_anim_stops_map old_anim_stops_map
        olds = Map.toList $ Map.map stop $ Map.difference old_anim_stops_map new_anim_stops_map
        in map snd $ news ++ olds ++ merges

-- |
-- Renders all of the objects in the visible objects table, detecting
-- diffs between this and the old table and rendering animations to
-- show state transitions.
--
renderObjects :: IORef RoguestarGlobals -> IO ()
renderObjects globals_ref =
    do anims <- getObjectAnimations globals_ref
       time <- getTime
       let object_reps = map (animateObject time) anims
       mapM_ (\x -> renderObject (object_rep_model x) globals_ref Poor x) object_reps

objectAnimationStop :: (String,Integer,Integer,String) -> Double -> RoguestarTable -> ObjectAnimationStop
objectAnimationStop obj_loc_data altitude details =
    ObjectAnimationStop { object_anim_stop_object_rep = objectRepresentation obj_loc_data altitude details,
                          object_anim_stop_uptodate = table_created details }

objectRepresentation :: (String,Integer,Integer,String) -> Double -> RoguestarTable -> ObjectRepresentation
objectRepresentation (uid,x,y,facing) altitude details =
    ObjectRepresentation { object_rep_uid = uid,
                           object_rep_model = fromMaybe "question_mark" $ 
                               listToMaybe $ catMaybes $ map (tableLookup details ("property","value")) 
                                   ["species","tool"],
                           object_rep_altitude = altitude,
                           object_rep_position = (fromInteger x,fromInteger y),
                           object_rep_heading_degrees = facingToAngle facing }

facingToAngle :: String -> Angle
facingToAngle "here" = zero_angle
facingToAngle "north" = zero_angle
facingToAngle "northeast" = degrees 45
facingToAngle "east" = degrees 90
facingToAngle "southeast" = degrees $ 90+45
facingToAngle "south" = degrees 180
facingToAngle "southwest" = degrees $ 180+45
facingToAngle "west" = degrees $ 270
facingToAngle "northwest" = degrees 270+45
facingToAngle unexpected = error $ "unexpected facing: " ++ unexpected

renderObject :: String -> IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
renderObject "encephalon" = renderEncephalon
renderObject "recreant" = basicRenderObject Recreant
renderObject "phase_pistol" = renderGroundedObject PhasePistol
renderObject _ = renderQuestionMark

basicRenderObject :: LibraryModel -> IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
basicRenderObject m globals_ref q object_rep =
    atObjectPosition object_rep $ do displayLibraryModel globals_ref m q

atObjectPosition :: ObjectRepresentation -> IO () -> IO ()
atObjectPosition object_rep@(ObjectRepresentation { object_rep_position = (x,y) }) fn =
    do preservingMatrix $ do GL.translate $ Vector3 x (object_rep_altitude object_rep) y
                             GL.rotate (inDegrees $ object_rep_heading_degrees object_rep) (Vector3 0 1 0 :: Vector3 Double)
                             fn

renderQuestionMark :: IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
renderQuestionMark globals_ref q (ObjectRepresentation { object_rep_position = (x,y) }) = 
    do camera <- liftM (fromCSN world_coordinates) $ getCamera globals_ref
       lookAtCamera camera (Point3D x 0.5 y) (displayLibraryModel globals_ref QuestionMark q)

renderEncephalon :: IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
renderEncephalon globals_ref q object_rep = 
    atObjectPosition object_rep $ 
        do displayLibraryModel globals_ref Encephalon q
           renderJoint globals_ref Encephalon q $ joint encephalon_joint_params
           renderJoint globals_ref Encephalon q $ joint $ leftSide encephalon_joint_params

renderGroundedObject :: LibraryModel -> IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
renderGroundedObject m globals_ref q object_rep@(ObjectRepresentation { object_rep_position = (x,y) }) =
    atObjectPosition object_rep $ do GL.translate (Vector3 0 0.1 0 :: Vector3 Double)
                                     GL.rotate (3*(x+y)*22.5 :: Double) (Vector3 0 1 0 :: Vector3 Double)
                                     GL.rotate (45 :: Double) (Vector3 0 0 1 :: Vector3 Double)
                                     displayLibraryModel globals_ref m q
                                     
-- |
-- Render a joint for the specified creature model.
--
renderJoint :: IORef RoguestarGlobals -> LibraryModel -> Quality -> Joint -> IO ()
renderJoint globals_ref jointed_creature q the_joint =
    do transform (joint_shoulder the_joint) $ displayLibraryModel globals_ref (Shoulder jointed_creature) q
       transform (joint_arm the_joint) $ displayLibraryModel globals_ref (Arm jointed_creature) q