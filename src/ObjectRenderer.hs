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
import Models.LibraryData
import Models.Library
import CameraTracking

data ObjectRepresentation = ObjectRepresentation { object_rep_model :: String,
                                                   object_rep_position :: (Float,Float),
                                                   object_rep_heading_degrees :: Float }

-- |
-- Gets the "visible-objects" table and renders it on the screen.
--
renderObjects :: IORef RoguestarGlobals -> IO ()
renderObjects globals_ref =
    do table <- driverGetTable globals_ref Anything "visible-objects" "0"
       when (isJust table) $ renderVisibleObjectsTable globals_ref $ fromJust table

renderVisibleObjectsTable :: IORef RoguestarGlobals -> RoguestarTable -> IO ()
renderVisibleObjectsTable globals_ref table = 
    do let object_position_data = tableSelect3Integer table ("object-unique-id","x","y") :: [(String,(Maybe Integer,Maybe Integer))]
       let objects_with_facing = zipWith (\(unique_id,pos) facing -> (unique_id,pos,facing)) object_position_data $ tableSelect1 table "facing" :: [(String,(Maybe Integer,Maybe Integer),String)]
       object_reps <- liftM (catMaybes) $ mapM (getObjectRepresentation globals_ref) objects_with_facing :: IO [ObjectRepresentation]
       mapM (renderObject globals_ref) object_reps
       return ()

getObjectRepresentation :: IORef RoguestarGlobals -> (String,(Maybe Integer,Maybe Integer),String) -> IO (Maybe ObjectRepresentation)
getObjectRepresentation globals_ref (object_id,(Just x,Just y),facing) =
    do object_details <- driverGetTable globals_ref Anything "object-details" object_id
       return $ (Just . objectRepresentation (x,y) facing) =<< object_details
getObjectRepresentation _ _ = return Nothing

objectRepresentation :: (Integer,Integer) -> String -> RoguestarTable -> ObjectRepresentation
objectRepresentation (x,y) facing details =
    ObjectRepresentation { object_rep_model = fromMaybe "question_mark" $ tableLookup details ("property","value") "species",
                           object_rep_position = (fromInteger x,fromInteger y),
                           object_rep_heading_degrees = facingToDegrees facing }

facingToDegrees :: String -> Float
facingToDegrees "here" = 0.0
facingToDegrees "north" = 0.0
facingToDegrees "northeast" = 45
facingToDegrees "east" = 90
facingToDegrees "southeast" = 90+45
facingToDegrees "south" = 180
facingToDegrees "southwest" = 180+45
facingToDegrees "west" = 270
facingToDegrees "northwest" = 270+45
facingToDegrees unexpected = error $ "unexpected facing: " ++ unexpected

toLibraryModel :: String -> LibraryModel
toLibraryModel "encephalon" = Encephalon
toLibraryModel unexpected = error $ "unexpected library: " ++ unexpected

renderObject :: IORef RoguestarGlobals -> ObjectRepresentation -> IO ()
renderObject globals_ref (ObjectRepresentation { object_rep_model = "question_mark", object_rep_position = (x,y) }) = 
    do camera <- liftM global_camera $ readIORef globals_ref
       lookAtCamera camera (Point3D x 0.5 y) (displayLibraryModel globals_ref QuestionMark Super)
renderObject globals_ref object_rep@(ObjectRepresentation { object_rep_position = (x,y) }) =
    do preservingMatrix $ do GL.translate $ Vector3 x 0 y
                             GL.rotate (object_rep_heading_degrees object_rep) (Vector3 0 1 0 :: Vector3 Float)
                             displayLibraryModel globals_ref (toLibraryModel $ object_rep_model object_rep) Super