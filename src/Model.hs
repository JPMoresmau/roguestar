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
    (Model.Color,
     Point3D,
     Point2D,
     Vector3D,
     Model(Union),
     rgbColor,
     flatTriangle,
     toTriangles,
     strip,
     toPOV,
     toOpenGL,
     frame,
     sor,
     transformation,
     goblet)
    where

import Data.Maybe
import Data.List
import ListUtils
import Math3D
import Data.Ratio
import Graphics.Rendering.OpenGL.GL

data RGB = RGB { red, green, blue :: Float }
	 deriving (Read,Show)
data Color = Color { rgb :: Model.RGB, alpha :: Maybe Float, shine :: Maybe Float, lum :: Maybe Model.RGB }
	   deriving (Read,Show)

instance Xyz RGB where
    toXYZ c = (red c,green c,blue c)

data Model = Triangle Model.Color (Point3D,Vector3D) (Point3D,Vector3D) (Point3D,Vector3D)
	   | Strip Model.Color [((Point3D,Vector3D),(Point3D,Vector3D))]
	   | Union [Model]
	   | Transformation (Math3D.Matrix Float) Model
	   deriving (Show)

rgbColor :: (Float,Float,Float) -> Model.Color
rgbColor (r,g,b) = Model.Color { rgb=Model.RGB r g b, alpha=Nothing, shine=Nothing, lum=Nothing }

-- |
-- A boring triangle with a single color and an automatically calculated normal vector.
--
flatTriangle :: Model.Color -> Point3D -> Point3D -> Point3D -> Model
flatTriangle c p1 p2 p3 = 
    let n = planeNormal p1 p2 p3
	in Triangle c (p1,n) (p2,n) (p3,n)

-- |
-- Generates normal vectors for initial, final, and each interior edge of a quadralateral strip.
--
quadStripToNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToNormals pts = let flat_normals = quadStripToFlatNormals pts
			     smooth_normals = quadStripToSmoothNormals pts
			     first_normal = head $ flat_normals
			     last_normal = last $ flat_normals
			     in [first_normal] ++ smooth_normals ++ [last_normal]

-- |
-- Generates a normal for each interior edge in a quadralateral strip.
-- If there are n vertex pairs, there will be n-2 normals.
--
quadStripToSmoothNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToSmoothNormals pts = map vectorAverage $ consecutives 2 $ quadStripToFlatNormals pts

-- |
-- Generates a normal for each face in a quadralateral strip.  If there are n vertex pairs,
-- there will be n-1 normals.
--
quadStripToFlatNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToFlatNormals [] = []
quadStripToFlatNormals [(_,_)] = []
quadStripToFlatNormals ((pl0,pr0):(pl1,pr1):pts) = (vectorNormalize $ newell [pl0,pr0,pr1,pl1]):(quadStripToFlatNormals ((pl1,pr1):pts))

-- |
-- Generates a strip of polygons.  A sequence of quadrilaterals are threaded through
-- two polylines, as GL_QUAD_STRIP.  Normals are automatically calculated.
--
strip :: Model.Color -> [(Point3D,Point3D)] -> Model
strip c pts = let normals = quadStripToNormals pts
		  (pts_l,pts_r) = unzip pts
		  result = zip (zip pts_l normals) (zip pts_r normals)
		  in Strip c result

-- |
-- Converts the specified Model into a Model built entirely of triangles.
--
-- Implementation note: careful, this is mutually-recursive with modelTransform
--
toTriangles :: Model -> Model
toTriangles triangle@(Triangle {}) = triangle
toTriangles (Strip c pts) = 
    let quadToTriangles double = [(Triangle c 
				   (fst $ fst $ fst double,snd $ fst $ fst double)
				   (fst $ fst $ snd double,snd $ fst $ snd double)
				   (fst $ snd $ fst double,snd $ snd $ fst double)),
				  (Triangle c
				   (fst $ fst $ snd double,snd $ fst $ snd double)
				   (fst $ snd $ snd double,snd $ snd $ snd double)				   
				   (fst $ snd $ fst double,snd $ snd $ fst double))]
	in Union $ concat $ map quadToTriangles $ doubles pts
toTriangles (Union things) = Union $ map toTriangles things
toTriangles (Transformation mat thing) = toTriangles $ modelTransform mat thing

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
    "  finish { ambient rgb <" ++ (vectorString $ fromMaybe (Model.RGB 0 0 0) (lum c)) ++ "> }\n" ++
    "}"
toPOV the_strip@(Strip {}) = toPOV $ toTriangles the_strip
toPOV (Union things) = unlines $ map toPOV things
toPOV (Transformation mat thing) = toPOV $ modelTransform mat thing

modelTransform :: Math3D.Matrix Float -> Model -> Model
modelTransform mat (Triangle c (p0,v0) (p1,v1) (p2,v2)) =
    seq (aMByNMatrix "in Model.modelTransform" 4 4 mat) $
    Triangle c (transform mat p0,transform mat v0) (transform mat p1,transform mat v1) (transform mat p2,transform mat v2)
modelTransform mat anything_else = modelTransform mat $ toTriangles anything_else

goblet :: Model
goblet = sor (rgbColor (0.5,0.75,0.75)) 50 $
	 points2d [(0.1,-1),
		   (0.5,0),
		   (1.0,2),
		   (1.1,2.1),
		   (1.2,2),
		   (0.85,0.75),
		   (0.5,-0.5),
		   (0.2,-1),
		   (0.1,-2),
		   (0.2,-2.1),
		   (0.4,-2.125),
		   (0.6,-2.15),
		   (1.0,-2.2),
		   (1.1,-2.3),
		   (0.1,-2.3)]

-- |
-- Constructs a surface of revolution.  In a SOR, a two-dimensional frame is rotated around
-- the Y-axis.  This is the computer graphics equivalent of a potter's wheel.
-- The second parameter indicates the number of subdivisions.  The third parameter
-- indicates the 2-dimensional frame.
--
sor :: Model.Color -> Integer -> [Point2D] -> Model
sor _ subdivisions _ | subdivisions < 3 = error "sor: requires at least 3 subdivisions"
sor c subdivisions pts = let radian_increments = reverse $ map (\x -> 2*pi*(fromInteger x)/(fromInteger subdivisions)) [0..(subdivisions-1)] :: [Float]
			     mats = map (\x -> rotationMatrix (Vector3D 0 1 0) x) radian_increments
			     p3ds = map (\x -> map (transform x :: Point2D -> Point3D) pts) mats
			     in frame c p3ds

-- |
-- Contructs a surface from a sequence of polyline tails.  This is like wrapping cloth around a wire frame.
-- 
frame :: Model.Color -> [[Point3D]] -> Model
frame c pts = let normals_smooth_one_way = map ((quadStripToNormals).(uncurry zip)) $ loopedDoubles pts :: [[Vector3D]]
		  normals_smooth_both_ways = map (map vectorAverage . transpose) $ loopedConsecutives 2 normals_smooth_one_way
		  in Union $ map (Strip c . uncurry zip) $ loopedDoubles $ map (uncurry zip) $ zip pts ((last normals_smooth_both_ways) : normals_smooth_both_ways)

-- |
-- Constructs a transformation of a Model.
--
transformation :: Math3D.Matrix Float -> Model -> Model
transformation mat model = Transformation (aMByNMatrix "in Model.transformation" 4 4 mat) model

toVertex3 :: Point3D -> Vertex3 Float
toVertex3 (Point3D x y z) = Vertex3 x y z

toNormal3 :: Vector3D -> Normal3 Float
toNormal3 (Vector3D x y z) = Normal3 x y z

toColor4_rgb :: Model.Color -> Color4 Float
toColor4_rgb (Model.Color { rgb=(Model.RGB r g b) }) = Color4 r g b 0

colorOpenGL :: Model.Color -> IO ()
colorOpenGL c = do materialShininess Front $= 0
		   materialSpecular Front $= Color4 0 0 0 1
		   materialDiffuse Front $= toColor4_rgb c
		   materialAmbient Front $= toColor4_rgb c

toOpenGL :: Model -> IO ()
toOpenGL (Triangle c (p1,v1) (p2,v2) (p3,v3)) =
    renderPrimitive Triangles drawTriangle
	where drawTriangle = do colorOpenGL c
				normal $ toNormal3 v1
				vertex $ toVertex3 p1
				normal $ toNormal3 v2
				vertex $ toVertex3 p2
				normal $ toNormal3 v3
				vertex $ toVertex3 p3
toOpenGL (Strip c the_strip) =
    renderPrimitive QuadStrip $ drawStrip the_strip
	where drawStrip pts = do colorOpenGL c
				 drawStrip_ pts
	      drawStrip_ pts@(((p0,v0),(p1,v1)):((p2,v2),(p3,v3)):_) =
		  do normal $ toNormal3 v0
		     vertex $ toVertex3 p0
		     normal $ toNormal3 v1
		     vertex $ toVertex3 p1
		     normal $ toNormal3 v2
		     vertex $ toVertex3 p2
		     normal $ toNormal3 v3
		     vertex $ toVertex3 p3
		     drawStrip_ $ tail pts
	      drawStrip_ ((_,_):[]) = return ()
	      drawStrip_ [] = return ()
toOpenGL (Union things) = mapM_ toOpenGL things
toOpenGL (Transformation mat thing) = 
    preservingMatrix render
	where render = do mat' <- newMatrix RowMajor $ concat $ rowMajorForm mat
			  multMatrix (mat' :: GLmatrix Float)
			  toOpenGL thing