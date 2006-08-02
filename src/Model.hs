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

module Model
    (Color,
     Point3D,
     Point2D,
     Vector3D,
     Model(Triangle,Union),
     flatTriangle,
     strip,
     toPOV)
    where

import Data.Maybe
import ListUtils
import Math3D
import Data.Ratio

data RGB = RGB { red, green, blue :: Float }
	 deriving (Read,Show)
data Color = Color { rgb :: RGB, alpha :: Maybe Float, lum :: Maybe RGB }
	   deriving (Read,Show)

instance Xyz RGB where
    toXYZ rgb = (red rgb,green rgb,blue rgb)

data Model = Triangle Color (Point3D,Vector3D) (Point3D,Vector3D) (Point3D,Vector3D)
	   | Strip Color [((Point3D,Vector3D),(Point3D,Vector3D))]
	   | Union [Model]
	   deriving (Read,Show)

rgbColor :: (Float,Float,Float) -> Color
rgbColor (r,g,b) = Color { rgb=RGB r g b, alpha=Nothing, lum=Nothing }

-- |
-- A boring triangle with a single color and an automatically calculated normal vector.
--
flatTriangle :: Color -> Point3D -> Point3D -> Point3D -> Model
flatTriangle c p1 p2 p3 = 
    let n = normal p1 p2 p3
	in Triangle c (p1,n) (p2,n) (p3,n)

-- |
-- Generates a normal for each interior edge in a quadralateral strip.
-- If there are n vertex pairs, there will be n-2 normals.
--
quadStripToSmootheNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToSmootheNormals pts = map vectorAverage $ consecutives 2 $ quadStripToFlatNormals pts

-- |
-- Generates a normal for each face in a quadralateral strip.  If there are n vertex pairs,
-- there will be n-1 normals.
--
quadStripToFlatNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToFlatNormals [(pl0,pr0)] = []
quadStripToFlatNormals ((pl0,pr0):(pl1,pr1):pts) = (newell [pl0,pr0,pr1,pl1]):(quadStripToFlatNormals ((pl1,pr1):pts))

-- |
-- Generates a strip of polygons.  A sequence of quadrilaterals are threaded through
-- two polylines, as GL_QUAD_STRIP.  Normals are automatically calculated.
--
strip :: Color -> [(XYZ,XYZ)] -> Model
strip c rawpts = let pts = map (\x -> (point3d $ fst x,point3d $ snd x)) rawpts
		     smoothe_normals = quadStripToSmootheNormals pts
		     flat_normals = quadStripToFlatNormals pts
		     first_normal = head $ flat_normals
		     last_normal = last $ flat_normals
		     all_normals = [first_normal] ++ smoothe_normals ++ [last_normal]
		     (pts_l,pts_r) = unzip pts
		     result = zip (zip pts_l all_normals) (zip pts_r all_normals)
		     in Strip c result

-- |
-- Converts the specified Model into a Model built entirely of triangles.
--
toTriangles :: Model -> Model
toTriangles triangle@(Triangle {}) = triangle
toTriangles (Strip color pts) = 
    let quadToTriangles double = [(Triangle color 
				   (fst $ fst $ fst double,snd $ fst $ fst double)
				   (fst $ fst $ snd double,snd $ fst $ snd double)
				   (fst $ snd $ fst double,snd $ snd $ fst double)),
				  (Triangle color
				   (fst $ fst $ snd double,snd $ fst $ snd double)
				   (fst $ snd $ snd double,snd $ snd $ snd double)				   
				   (fst $ snd $ fst double,snd $ snd $ fst double))]
	in Union $ foldr1 (++) $ map quadToTriangles $ doubles pts

-- |
-- Converts the specified Model into POV-Ray source.
--
toPOV :: Model -> String
toPOV (Triangle c (p0,v0) (p1,v1) (p2,v2)) =
    "smooth_triangle {\n" ++
    "  <" ++ (vectorString p0) ++ 
    ">,<" ++ (vectorString v0) ++ 
    ">,<" ++ (vectorString p1) ++
    ">,<" ++ (vectorString v1) ++ 
    ">,<" ++ (vectorString p2) ++
    ">,<" ++ (vectorString v2) ++ ">\n" ++
    "  pigment { color rgbt <" ++ (vectorString $ rgb c) ++ 
    "," ++ (show $ fromMaybe 0.0 (alpha c)) ++ "> }\n" ++
    "  finish { ambient rgb <" ++ (vectorString $ fromMaybe (RGB 0 0 0) (lum c)) ++ "> }\n" ++
    "}"
toPOV strip@(Strip {}) = toPOV $ toTriangles strip
toPOV (Union things) = unlines $ map toPOV things

printLittleModel :: IO ()
printLittleModel = putStr $ toPOV $ strip (rgbColor (1,0,0)) [((0,0,-1),(0,0,1)), ((1,0,-1),(1,0,1)), ((2,1,-1),(2,1,1))]

printSimpleSORModel :: IO ()
printSimpleSORModel = putStr $ toPOV $ sor (rgbColor (1.0,0.5,0.25)) 5
		      [(1,1),
		       (4,4),
		       (2,1)]

printSORModel :: IO ()
printSORModel = putStr $ toPOV $ sor (rgbColor (0.5,0.75,0.75)) 12
		[(0.1,-1),
		 (0.5,0),
		 (1.0,2),
		 (1.1,2.1),
		 (1.2,2),
		 (0.5,-0.5),
		 (0.2,-1),
		 (0.1,-2),
		 (0.2,-2.1),
		 (1.0,-2.2),
		 (1.1,-2.3),
		 (0.1,-2.3)]

-- |
-- Constructs a surface of revolution.  In a SOR, a two-dimensional frame is rotated around
-- the Y-axis.  This is the computer graphics equivalent of a potter's wheel.
-- The second parameter indicates the number of subdivisions.  The third parameter
-- indicates the 2-dimensional frame.
--
sor :: Color -> Integer -> [(Float,Float)] -> Model
sor c subdivisions _ | subdivisions < 3 = error "sor: requires at least 3 subdivisions"
sor c subdivisions pairs = let radians = map (\x -> pi*(fromInteger x)/(fromInteger subdivisions)) [0..(subdivisions-1)] :: [Float]
			       mats = map (\x -> rotationMatrix (Vector3D 0 1 0) x) radians
			       points2d = map point2d pairs
			       points3d = map (\x -> map (toXYZ . (transform x :: Point2D -> Point3D)) points2d) mats
			       in Union $ map ((strip c).(uncurry zip)) $ loopedDoubles points3d
