{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

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
    (Material(..),
     Texture(..),
     Point3D,
     Point2D,
     Vector3D,
     proceduralTexture,
     Model(Union),
     modelSize,
     scaleModelFactor,
     scaleModel,
     rgbColor,
     rgbShine,
     rgbLum,
     enhancePoints,
     flatTriangle,
     strip,
     toOpenGL,
     dropUnionElements,
     frame,
     extrude,
     sor,
     deformedSor,
     radianIncrements)
    where

import System.Random
import Data.Maybe
import Data.List
import ListUtils
import Math3D
import Data.Ratio
import Graphics.Rendering.OpenGL.GL

data RGB = RGB { red, green, blue :: Float } deriving (Show)
                
instance Lerpable Model.RGB Float where
    lerp u (a,b) = Model.RGB { red = lerp u (red a,red b),
                               green = lerp u (green a,green b),
                               blue = lerp u (blue a,blue b) }
                
data Material = Material { rgb :: Model.RGB, alpha :: Maybe Float, shine :: Maybe Float, lum :: Maybe Model.RGB } deriving (Show)

instance Lerpable Material Float where
    lerp u (a,b) = Material { rgb = lerp u (rgb a,rgb b),
                              alpha = lerp u (alpha a,alpha b),
                              shine = lerp u (shine a,shine b),
                              lum = lerp u (lum a,lum b) }

data Texture = SolidTexture Material
             | ProceduralTexture (Point3D -> Material)
                
instance AffineTransformable Texture where
    transform _ (SolidTexture mat) = SolidTexture mat
    transform m (ProceduralTexture fn) = ProceduralTexture (\p -> fn $ transform (matrixInverse m) p) -- note: inferior asymptotic complexity

instance Show Texture where
    show (SolidTexture mat) = "SolidTexture " ++ show mat
    show (ProceduralTexture _) = "ProceduralTexture _"

instance Xyz RGB where
    toXYZ c = (red c,green c,blue c)

data Model = Triangle Texture (Point3D,Vector3D) (Point3D,Vector3D) (Point3D,Vector3D)
	   | Strip Texture [((Point3D,Vector3D),(Point3D,Vector3D))]
	   | Union [Model]
	   | Transformation (Math3D.Matrix Float) Model
	   deriving (Show) -- only important for debugging, remember model designers might want to look at this

-- |
-- Essentially the maximum distance to any point in a model from the model's origin.
--
modelSize :: Model -> Float
modelSize (Triangle _ (p1,_) (p2,_) (p3,_)) = maximum $ map (distanceBetween origin_point_3d) [p1,p2,p3]
modelSize (Strip _ pts) = maximum $ map (distanceBetween origin_point_3d) $ concatMap (\((p1,_),(p2,_)) -> [p1,p2]) pts
modelSize (Union models) = maximum $ map modelSize models
modelSize transformedModel@(Transformation {}) = modelSize $ pretransform (identityMatrix 4) transformedModel

-- |
-- Fully apply the specified affine transform to this matrix, now, along with any transformations
-- stored in the model.  Afterwards the model will have been stripped of all Transformation elements.
--
pretransform :: Math3D.Matrix Float -> Model -> Model
pretransform m (Transformation m' model) = pretransform (matrixMultiply m m') model
pretransform m (Triangle tex (p1,v1) (p2,v2) (p3,v3)) = 
    Triangle (transform m tex) (transform m p1,vectorNormalize $ transform m v1) (transform m p2,vectorNormalize $ transform m v2) (transform m p3,vectorNormalize $ transform m v3)
pretransform m (Strip tex pts) = 
    Strip (transform m tex) $ map (\((p1,v1),(p2,v2)) -> ((transform m p1,vectorNormalize $ transform m v1),(transform m p2,vectorNormalize $ transform m v2))) pts
pretransform m (Union models) = Union $ map (pretransform m) models

-- |
-- The scale factor that would be used by scaleModel to make this model the specified size.
--
scaleModelFactor :: Float -> Model -> Float
scaleModelFactor scalar model = scalar / modelSize model

-- |
-- Scale the model to be the specified size, as given by modelSize.
--
scaleModel :: Float -> Model -> Model
scaleModel scalar model = Math3D.scale' (scaleModelFactor scalar model) model

-- |
-- Generates a Color from red, green, and blue components.  The color has no alpha, no shinyness,
-- and no luminance.
--
rgbColor :: (Float,Float,Float) -> Material
rgbColor (r,g,b) = Material { rgb=Model.RGB r g b, alpha=Nothing, shine=Nothing, lum=Nothing }

-- |
-- As rgbColor, but the first parameter (between 0 and 1) represents specular reflection.
--
rgbShine :: Float -> (Float,Float,Float) -> Material
rgbShine the_shine (r,g,b) = Material { rgb=Model.RGB r g b, alpha=Nothing, shine=Just the_shine, lum=Nothing }

rgbLum :: Float -> (Float,Float,Float) -> Material
rgbLum l (r,g,b) = Material { rgb=Model.RGB r g b,
			      alpha=Nothing,
			      shine=Nothing,
			      lum=Just $ Model.RGB (l*r) (l*g) (l*b) }

-- |
-- Combine a noise function with a material map to make a procedural texture.
--
proceduralTexture :: NoiseFunction -> [(Float,Material)] -> Texture
proceduralTexture nf material_map = ProceduralTexture (\p -> lerpMap (noiseAt nf (identityMatrix 4,p)) material_map)

-- |
-- Takes a grid of points, as for a frame object, and uses a simple
-- interpolation function to increase the number of vertices without changing
-- the shape of the model.
--
enhancePoints :: [[Point3D]] -> [[Point3D]]
enhancePoints = transpose . map loopedInterpolateBetween3d . transpose . map interpolateBetween3d

-- |
-- A boring triangle with a single color and an automatically calculated normal vector.
--
flatTriangle :: Texture -> Point3D -> Point3D -> Point3D -> Model
flatTriangle tex p1 p2 p3 = 
    let n = planeNormal p1 p2 p3
	in Triangle tex (p1,n) (p2,n) (p3,n)

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
-- If there are n vertex pairs, there will be n minus 2 normals.
--
quadStripToSmoothNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToSmoothNormals pts = map vectorAverage $ consecutives 2 $ quadStripToFlatNormals pts

-- |
-- Generates a normal for each face in a quadralateral strip.  If there are n vertex pairs, then there will be n minus 1 normals.
--
quadStripToFlatNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToFlatNormals [] = []
quadStripToFlatNormals [(_,_)] = []
quadStripToFlatNormals ((pl0,pr0):(pl1,pr1):pts) = (vectorNormalize $ newell [pl0,pr0,pr1,pl1]):(quadStripToFlatNormals ((pl1,pr1):pts))

-- |
-- Generates a strip of polygons.  A sequence of quadrilaterals are threaded through
-- two polylines, as 'GL_QUAD_STRIP'.  Normals are automatically calculated.
--
strip :: Texture -> [(Point3D,Point3D)] -> Model
strip tex pts = let normals = quadStripToNormals pts
		    (pts_l,pts_r) = unzip pts
		    result = zip (zip pts_l normals) (zip pts_r normals)
		    in Strip tex result

-- |
-- Skips some elements of a union at random.  The first parameter is the number
-- of elements out of 100 that should be included (not dropped).  The second
-- is a random seed, and the third is a model of the Union form.
--
dropUnionElements :: Int -> StdGen -> Model -> Model
dropUnionElements percent _ _ | percent > 100 = error "dropUnionElements: percent > 100"
dropUnionElements percent _ _ | percent < 0 = error "dropUnionElements: percent < 100"
dropUnionElements _ _ (Union []) = Union []
dropUnionElements percent rand_ints (Union things) = let (next_int,next_gen) = next rand_ints
							 (Union rest) = dropUnionElements percent next_gen (Union $ tail things)
							 in if (next_int `mod` 100 < percent)
							    then Union ((head things) : rest)
							    else Union rest
dropUnionElements _ _ _ = error "dropUnionElements: Model not of form Union {}"

-- |
-- Evenly spaced numbers from 0 to 2*pi, in a list whose length is specified by the parameter.
--
radianIncrements :: (Floating a) => Integer -> [a]
radianIncrements subdivisions = map ((2*pi*) . (/ fromInteger subdivisions) . fromInteger) [0 .. subdivisions - 1]

-- |
-- Extrude a figure, usually in the XY plane, along a polyline.
--
-- extrude mat subdivisions up figure polyline
--
-- [subdivisions] is the number of frames of the figure that will be generated
--   this is completely independent of the number of vertices in the polylines
--   therefore the polyline can be extremely intricate, but you must increase 
--   the number of subdivisions to actually model that intricacy. 
-- [up] the figure will always be oriented so that it's +Y axis is rotated to point as much as possible toward the up vector
-- [figure] is a list of vertices of a figure to be extruded
-- [polyline] is a list of (vertex,thickness) pairs, where vertex is a vertex of the polyline and
--   thickness is a scaling factor (usually 1.0) representing how large the figure should be at that vertex.
--
extrude :: Texture -> Integer -> Vector3D -> [Point3D] -> [(Point3D,Float)] -> Model
extrude tex subdivs up figure polyline =
  let points = map (\subdiv -> extrude_ subdiv subdivs up figure polyline) [0..subdivs]
      in frame tex $ transpose points

extrude_ :: Integer -> Integer -> Vector3D -> [Point3D] -> [(Point3D,Float)] -> [Point3D]
extrude_ 0 _ up figure polyline = extrude1Frame 0.0 up figure $ head $ doubles polyline
extrude_ subdiv subdivs up figure polyline | subdiv == subdivs = extrude1Frame 1.0 up figure $ last $ doubles polyline
extrude_ subdiv subdivs up figure polyline =
  let segments = doubles polyline
      distance_into_polyline = ((subdiv * genericLength segments) % subdivs)
      polyline_index = max 0 $ min (genericLength segments) $ floor distance_into_polyline
      segment = segments `genericIndex` polyline_index
      in extrude1Frame (fromRational distance_into_polyline - fromInteger polyline_index) up figure segment

-- |
-- Produces a single extruded frame.
--
extrude1Frame :: Float -> Vector3D -> [Point3D] -> ((Point3D,Float),(Point3D,Float)) -> [Point3D]
extrude1Frame fraction_of_segment up figure ((a,a_scale),(b,b_scale)) = 
  let to_point = vectorToFrom (Math3D.translate (vectorScaleTo (fraction_of_segment * distanceBetween a b) $ vectorToFrom b a) a) origin_point_3d
      scale_factor = fraction_of_segment * b_scale + (1 - fraction_of_segment) * a_scale
      z_vector = vectorScaleTo scale_factor $ vectorToFrom b a
      x_vector = vectorScaleTo scale_factor $ crossProduct z_vector up
      y_vector = vectorScaleTo scale_factor $ crossProduct x_vector z_vector
      transformation_matrix = xyzMatrix x_vector y_vector z_vector
      in map (Math3D.translate to_point) $ (map (transform transformation_matrix) figure :: [Point3D])     
                
-- |
-- Constructs a surface of revolution.  In a SOR, a two dimensional frame is rotated around
-- the Y axis.  This is the computer graphics equivalent of a potter's wheel.
-- The second parameter indicates the number of subdivisions.  The third parameter
-- is the two dimensional frame.
--
-- The result is always of the form Union [longitudinal elements].
--
sor :: Texture -> Integer -> [Point2D] -> Model
sor = deformedSor id

-- |
-- As a SOR, but apply a deformation function to all points in the SOR.
--
deformedSor :: (Point3D -> Point3D) -> Texture -> Integer -> [Point2D] -> Model
deformedSor _ _ subdivisions _ | subdivisions < 3 = error "sor: requires at least 3 subdivisions"
deformedSor dfn tex subdivisions pts = let points = map ((\m -> map (transformHomogenous m) pts) . rotationMatrix (Vector3D 0 1 0)) $ reverse $ radianIncrements subdivisions
			                   in frame tex $ map (map dfn) points

-- |
-- Contructs a surface from a sequence of polyline tails.  This is like wrapping cloth around a wire frame.
-- The result is always a union of elements.
-- 
frame :: Texture -> [[Point3D]] -> Model
frame tex pts = let normals_smooth_one_way = map ((quadStripToNormals).(uncurry zip)) $ loopedDoubles pts :: [[Vector3D]]
	    	    normals_smooth_both_ways = map (map vectorAverage . transpose) $ loopedConsecutives 2 normals_smooth_one_way
		    in Union $ map (Strip tex . uncurry zip) $ loopedDoubles $ map (uncurry zip) $ zip pts ((last normals_smooth_both_ways) : normals_smooth_both_ways)

instance AffineTransformable Model where
    transform mat model = Transformation (aMByNMatrix "in Model.transform" 4 4 mat) model

toVertex3 :: Point3D -> Vertex3 Float
toVertex3 (Point3D x y z) = Vertex3 x y z

toNormal3 :: Vector3D -> Normal3 Float
toNormal3 (Vector3D x y z) = Normal3 x y z

toColor4_rgb :: Material -> Color4 Float
toColor4_rgb (Material { rgb=(Model.RGB r g b), alpha=a }) = Color4 r g b (fromMaybe 1 a)

toColor4_lum :: Material -> Color4 Float
toColor4_lum (Material { lum=(Just (Model.RGB r g b))}) = Color4 r g b 0
toColor4_lum _ = Color4 0 0 0 0

materialToOpenGL :: Material -> IO ()
materialToOpenGL c = 
    let shininess = fromMaybe 0 (shine c)
	in do materialShininess Front $= shininess*128
	      materialSpecular Front $= Color4 shininess shininess shininess 1.0
	      materialAmbientAndDiffuse Front $= toColor4_rgb c
              materialEmission Front $= toColor4_lum c

colorOpenGL :: Texture -> IO ()
colorOpenGL (SolidTexture c) = materialToOpenGL c
colorOpenGL _ = return ()
                
colorOpenGLAt :: Texture -> Point3D -> IO ()
colorOpenGLAt (ProceduralTexture ptfn) pt = materialToOpenGL $ ptfn pt
colorOpenGLAt _ _ = return ()
                
toOpenGL :: Model -> IO ()
toOpenGL (Triangle tex (p1,v1) (p2,v2) (p3,v3)) =
    renderPrimitive Triangles drawTriangle
	where drawTriangle = do colorOpenGL tex
                                colorOpenGLAt tex p1
				normal $ toNormal3 v1
				vertex $ toVertex3 p1
                                colorOpenGLAt tex p2
				normal $ toNormal3 v2
				vertex $ toVertex3 p2
                                colorOpenGLAt tex p3
				normal $ toNormal3 v3
				vertex $ toVertex3 p3
toOpenGL (Strip tex the_strip) =
    renderPrimitive QuadStrip $ drawStrip the_strip
	where drawStrip pts = do colorOpenGL tex
				 drawStrip_ pts
	      drawStrip_ pts@(((p0,v0),(p1,v1)):((p2,v2),(p3,v3)):_) =
		  do colorOpenGLAt tex p0
                     normal $ toNormal3 v0
		     vertex $ toVertex3 p0
                     colorOpenGLAt tex p1
		     normal $ toNormal3 v1
		     vertex $ toVertex3 p1
                     colorOpenGLAt tex p2
		     normal $ toNormal3 v2
		     vertex $ toVertex3 p2
                     colorOpenGLAt tex p3
		     normal $ toNormal3 v3
		     vertex $ toVertex3 p3
		     drawStrip_ $ tail pts
	      drawStrip_ ((_,_):[]) = return ()
	      drawStrip_ [] = return ()
toOpenGL (Union things) = mapM_ toOpenGL things
toOpenGL (Transformation mat thing) = transform mat $ toOpenGL thing

-- This scares me.
instance AffineTransformable (IO a) where
    transform mat iofn = preservingMatrix $ do mat' <- newMatrix RowMajor $ concat $ rowMajorForm mat
                                               multMatrix (mat' :: GLmatrix Float)
                                               iofn