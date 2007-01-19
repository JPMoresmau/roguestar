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
import CameraTracking
import System.IO
import TerrainRenderer

data ObjectRepresentation = ObjectRepresentation { object_rep_uid :: String,
                                                   object_rep_model :: String,
                                                   object_rep_position :: (Float,Float),
                                                   object_rep_altitude :: Float,
                                                   object_rep_heading_degrees :: Angle }

data ObjectAnimationStop = ObjectAnimationStopFrame { object_anim_stop_object_rep :: ObjectRepresentation,
                                                      object_anim_stop_uptodate :: Seconds }

data ObjectAnimation = ObjectAnimation { object_anim_old :: ObjectRepresentation,
                                         object_anim_new :: ObjectRepresentation,
                                         object_anim_starts :: Seconds }

instance Lerpable ObjectRepresentation Float where
    lerp _ (a,b) | object_rep_uid a /= object_rep_uid b = error "ObjectRepresentation; lerp: tried to lerp different objects"
    lerp u (a,b) = ObjectRepresentation { object_rep_uid = object_rep_uid a,
                                          object_rep_model = object_rep_model a,
                                          object_rep_position = lerp u (object_rep_position a,object_rep_position b),
                                          object_rep_altitude = lerp u (object_rep_altitude a,object_rep_altitude b),
                                          object_rep_heading_degrees = lerp u (object_rep_heading_degrees a,object_rep_heading_degrees b),
                                          object_rep_uptodate = max (object_rep_uptodate a) (object_rep_uptodate b) }

getObjectRepresentations :: IORef RoguestarGlobals -> IO [ObjectRepresentation]
getObjectRepresentations globals_ref =
    do new_objects <- format $ driverGetTable globals_ref New "visible-objects" "0"
       old_objects <- format $ driverGetTable globals_ref Old "visible-objects" "0"
       new_object_anim_stops <- mapM (positionInfoToObjectAnimStop roguestar_globals New) new_table
       old_object_anim_stops <- mapM (positionInfoToObjectAnimStop roguestar_globals Old) old_table
       mergeObjectRepresentations new_object_reps old_object_reps
         where format_tableSelect = tableSelectFormatted [TDString "object-unique-id",TDInteger "x",TDInteger "y",TDString "facing"]
               format_toTuple = \e -> case e of
                                             [TDString uid,TDInteger x,TDInteger y,TDString f] -> [(uid,x,y,f)]
                                             _ -> []
               format = maybe [] (concatMap format_toTuple . format_tableSelect)

positionInfoToObjectAnimStop :: IORef RoguestarGlobals -> DataFreshness -> (String,Integer,Integer,String) -> IO ObjectAnimationStop
positionInfoToObjectAnimStop globals_ref freshness (uid,x,y,f) =
    do object_details <- driverGetTable globals_ref freshness "object-details" uid
       altitue <- getTerrainHeight globals_ref (x,y)
       return $ (Just . objectAnimationStop uid (x,y) altitude facing) =<< object_details

-- |
-- Merge new and old animation stops to make an animation.
--
mergeObjectAnimations :: [ObjectAnimationStop] -> [ObjectAnimationStop] -> IO [ObjectAnimation]
mergeObjectAnimations new_anim_stops old_anim_stops =
    let old_object_reps_map = Map.fromList $ List.map ((\x -> (object_rep_uid,x)) . object_animation_stop_object_rep) old_object_reps
        merge new old = ObjectAnimation { object_anim_old = old,
                                          object_anim_new = object_anim_stop_object_rep new,
                                          object_anim_starts = object_anim_stop_uptodate new }

{-
-- |
-- Gets the "visible-objects" table and renders it on the screen.
--
renderObjects :: IORef RoguestarGlobals -> IO ()
renderObjects globals_ref =
    do new_table <- driverGetTable globals_ref New "visible-objects" "0"
       when (isJust table) $ renderVisibleObjectsTable globals_ref $ fromJust table

renderVisibleObjectsTable :: IORef RoguestarGlobals -> RoguestarTable -> IO ()
renderVisibleObjectsTable globals_ref table = 
    do let object_position_data = tableSelect3Integer table ("object-unique-id","x","y") :: [(String,(Maybe Integer,Maybe Integer))]
       let objects_with_facing = zipWith (\(unique_id,pos) facing -> (unique_id,pos,facing)) object_position_data $ tableSelect1 table "facing" :: [(String,(Maybe Integer,Maybe Integer),String)]
       object_reps <- liftM (catMaybes) $ mapM (getObjectRepresentation globals_ref) objects_with_facing :: IO [ObjectRepresentation]
       mapM (\x -> renderObject (object_rep_model x) globals_ref Super x) object_reps
       return ()

getObjectRepresentation :: IORef RoguestarGlobals -> (String,(Maybe Integer,Maybe Integer),String) -> IO (Maybe ObjectRepresentation)
getObjectRepresentation globals_ref (object_id,(Just x,Just y),facing) =
    do object_details <- driverGetTable globals_ref Anything "object-details" object_id
       altitude <- getTerrainHeight globals_ref (x,y)
       return $ (Just . objectRepresentation (x,y) altitude facing) =<< object_details
getObjectRepresentation _ _ = return Nothing
-}

objectAnimationStop :: (String,Integer,Integer,String) -> Float -> RoguestarTable -> ObjectAnimerationStop
objectAnimationStop obj_loc_data altitude details =
    ObjectAnimerationStop { object_anim_stop_object_rep = objectRepresentation obj_loc_data altitude details,
                            object_anim_stop_uptodate = table_created details }

objectRepresentation :: (String,Integer,Integer,String) -> Float -> RoguestarTable -> ObjectRepresentation
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
                             GL.rotate (inDegrees $ object_rep_heading_degrees object_rep) (Vector3 0 1 0 :: Vector3 Float)
                             fn

renderQuestionMark :: IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
renderQuestionMark globals_ref q (ObjectRepresentation { object_rep_position = (x,y) }) = 
    do camera <- liftM global_camera $ readIORef globals_ref
       lookAtCamera camera (Point3D x 0.5 y) (displayLibraryModel globals_ref QuestionMark q)

renderEncephalon :: IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
renderEncephalon globals_ref q object_rep = 
    atObjectPosition object_rep $ 
        do displayLibraryModel globals_ref Encephalon q
           renderJoint globals_ref Encephalon q $ joint encephalon_joint_params
           renderJoint globals_ref Encephalon q $ joint $ leftSide encephalon_joint_params

renderGroundedObject :: LibraryModel -> IORef RoguestarGlobals -> Quality -> ObjectRepresentation -> IO ()
renderGroundedObject m globals_ref q object_rep@(ObjectRepresentation { object_rep_position = (x,y) }) =
    atObjectPosition object_rep $ do GL.translate (Vector3 0 0.1 0 :: Vector3 Float)
                                     GL.rotate (3*(x+y)*22.5 :: Float) (Vector3 0 1 0 :: Vector3 Float)
                                     GL.rotate (45 :: Float) (Vector3 0 0 1 :: Vector3 Float)
                                     displayLibraryModel globals_ref m q
                                     
-- |
-- Render a joint for the specified creature model.
--
renderJoint :: IORef RoguestarGlobals -> LibraryModel -> Quality -> Joint -> IO ()
renderJoint globals_ref jointed_creature q the_joint =
    do transform (joint_shoulder the_joint) $ displayLibraryModel globals_ref (Shoulder jointed_creature) q
       transform (joint_arm the_joint) $ displayLibraryModel globals_ref (Arm jointed_creature) q