\section{Primitives for Modelling: RSAGL.PrimitiveModel}
\label{RSAGLPrimitiveModel}

RSAGL.PrimitiveModel simply allows objects to be modeled at a level very close to their OpenGL roots.
The Model ADT supports Triangle (GL_TRIANGLES) and quadralateral Strip (GL_QUAD_STRIP), along with simple
functions that can be used to generate useful models by combining those forms.

\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RSAGL.PrimitiveModel
    (Material(..),
     Texture(..),
     proceduralTexture,
     PrimitiveModel,
     modelSize,
     scaleModel,
     rgbColor,
     rgbShine,
     rgbLum,
     flatTriangle,
     strip,
     toOpenGL,
     frame,
     extrude,
     sor,
     deformedSor,
     RSAGL.PrimitiveModel.union,
     decomposeModel)
    where

import Data.Maybe
import Data.List
import RSAGL.ListUtils
import RSAGL.Vector
import RSAGL.Affine
import RSAGL.Matrix
import RSAGL.Homogenous
import RSAGL.Interpolation
import RSAGL.Angle
import RSAGL.Noise
import Data.Ratio
import Graphics.Rendering.OpenGL.GL as GL
\end{code}

\subsection{Defining Colors}

Colors are defined by the RGB data structure, with complement values in the range of 0.0 (dark) to 1.0 (bright).

\begin{code}
data RGB = RGB { red, green, blue :: Double } deriving (Show)

instance Xyz RGB where
    toXYZ c = (red c,green c,blue c)

instance Lerpable RSAGL.PrimitiveModel.RGB where
    lerp u (a,b) = RSAGL.PrimitiveModel.RGB { red = lerp u (red a,red b),
                                     green = lerp u (green a,green b),
                                     blue = lerp u (blue a,blue b) }
\end{code}

\subsection{Defining Materials}

\begin{code}
data Material = Material { rgb :: RSAGL.PrimitiveModel.RGB, 
                           alpha :: Maybe Double, 
                           shine :: Maybe Double, 
                           lum :: Maybe RSAGL.PrimitiveModel.RGB } deriving (Show)

instance Lerpable Material where
    lerp u (a,b) = Material { rgb = lerp u (rgb a,rgb b),
                              alpha = lerp u (alpha a,alpha b),
                              shine = lerp u (shine a,shine b),
                              lum = lerp u (lum a,lum b) }
\end{code}

rgbColor describes a simple material with a diffuse RGB color, no transparency, no shininess, and no luminance.

The use of Maybe types when dealing with alpha, shine, and lum allows us to optimize some operations, both
by not specifying the irrelevant information to OpenGL, and by, for example not using those values in interpolation
functions.

\begin{code}
rgbColor :: (Double,Double,Double) -> Material
rgbColor (r,g,b) = Material { rgb=RSAGL.PrimitiveModel.RGB r g b, 
                              alpha=Nothing, 
                              shine=Nothing, 
                              lum=Nothing }
\end{code}

rgbShine describes a material with color and shininess.  Shininess is in the range (0.0..1.0).

\begin{code}
rgbShine :: Double -> (Double,Double,Double) -> Material
rgbShine the_shine (r,g,b) = Material { rgb=RSAGL.PrimitiveModel.RGB r g b, 
                                        alpha=Nothing, 
                                        shine=Just the_shine, 
                                        lum=Nothing }
\end{code}

rgbLum describes a material with color and luminance.  

\begin{code}
rgbLum :: Double -> (Double,Double,Double) -> Material
rgbLum l (r,g,b) = Material { rgb=RSAGL.PrimitiveModel.RGB r g b,
			      alpha=Nothing,
			      shine=Nothing,
			      lum=Just $ RSAGL.PrimitiveModel.RGB (l*r) (l*g) (l*b) }
\end{code}

\subsection{Defining textures}

Texture can descrobe a sold texture or a procedural texture.  Procedural textures are typically generated using RSAGL.Noise.
\footnote{See page \pageref{RSAGLNoise}}

A procedural texture is generated at runtime and defined by a mathematical function that maps points in 3-space to colors.

\begin{code}
data Texture = SolidTexture Material
             | ProceduralTexture (Point3D -> Material)

instance AffineTransformable Texture where
    transform _ (SolidTexture mat) = SolidTexture mat
    transform m (ProceduralTexture fn) = ProceduralTexture (\p -> fn $ transform (matrixInverse m) p) -- note: inferior asymptotic complexity

instance Show Texture where
    show (SolidTexture mat) = "SolidTexture " ++ show mat
    show (ProceduralTexture _) = "ProceduralTexture _"
\end{code}

proceduralTexture combines a noise function with a material map to make a procedural texture.  A material map is like a description
of a piecewise linear function that must cover the entire range of the nosie function.  For example, for a rainbow pattern, you
might use:  [(0,red),(0.2,orange),(0.4,red),(0.6,green),(0.8,blue),(0.9,indigo),(1.0,violet)].

\begin{code}
proceduralTexture :: NoiseFunction -> [(Double,Material)] -> Texture
proceduralTexture nf material_map = ProceduralTexture (\p -> lerpMap (noiseAt nf p) material_map)
\end{code}

\subsection{Describing Models}

The Model data structure describes Models in the simplest forms OpenGL allows, either as triangles or quadralateral strips.
Models can also be unioned in a tree structure.

\begin{code}
data PrimitiveModel = Triangle Texture (Point3D,Vector3D) (Point3D,Vector3D) (Point3D,Vector3D)
	            | Strip Texture [((Point3D,Vector3D),(Point3D,Vector3D))]
	            | Union [Model]
	            | Transformation (RSAGL.Matrix.Matrix Double) PrimitiveModel
	   deriving (Show) -- only important for debugging, remember model designers might want to look at this
\end{code}

\subsection{Modelling with simple triangles}

flatTriangle describes a simple flat triangle.  Such triangles constructed edge-to-edge will be seen to have sharp edges.
Care must be taken that the triangle is correctly oriented, or backface culling will remove it from the scene entirely.
If this happens, or a shape seems to be inside out, simply reverse the order of the vertices.

\begin{code}
flatTriangle :: Texture -> Point3D -> Point3D -> Point3D -> PrimitiveModel
flatTriangle tex p1 p2 p3 = 
    let n = newell [p1,p2,p3]
	in Triangle tex (p1,n) (p2,n) (p3,n)
\end{code}

\subsection{Modelling with quadralateral strips}

Generates a strip of connected quadralaters defined by two polylines, as 'GL_QUAD_STRIP'.  Normals are automatically calculated.

\begin{code}
strip :: Texture -> [(Point3D,Point3D)] -> PrimitiveModel
strip tex pts = let normals = quadStripToNormals pts
		    (pts_l,pts_r) = unzip pts
		    result = zip (zip pts_l normals) (zip pts_r normals)
		    in Strip tex result
\end{code}

quadStripToNormals generates normal vectors for initial, final, and each interior edge of a quadralateral strip.

\begin{code}
quadStripToNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToNormals pts = let flat_normals = quadStripToFlatNormals pts
			     smooth_normals = quadStripToSmoothNormals pts
			     first_normal = head $ flat_normals
			     last_normal = last $ flat_normals
			     in [first_normal] ++ smooth_normals ++ [last_normal]
\end{code}

quadStripToSmoothNormals generates a normal for each interior edge in a quadralateral strip.
If there are n vertex pairs, there will be n minus 2 normals.

\begin{code}
quadStripToSmoothNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToSmoothNormals pts = map vectorAverage $ consecutives 2 $ quadStripToFlatNormals pts
\end{code}

quadStripToFlatNormals generates a normal for each face in a quadralateral strip.  If there are n vertex pairs, then there will be n minus 1 normals.

\begin{code}
quadStripToFlatNormals :: [(Point3D,Point3D)] -> [Vector3D]
quadStripToFlatNormals [] = []
quadStripToFlatNormals [(_,_)] = []
quadStripToFlatNormals ((pl0,pr0):(pl1,pr1):pts) = (vectorNormalize $ newell [pl0,pr0,pr1,pl1]):(quadStripToFlatNormals ((pl1,pr1):pts))
\end{code}

\subsection{Modelling with Extrusions}

Extrude a figure, usually in the XY plane, along a polyline.

\begin{description}
\item[subdivisions] is the number of frames of the figure that will be generated
   this is completely independent of the number of vertices in the polylines
   therefore the polyline can be extremely intricate, but you must increase 
   the number of subdivisions to actually model that intricacy. 
\item[up] the figure will always be oriented so that it's +Y axis is rotated to point as much as possible toward the up vector
\item[figure] is a list of vertices of a figure to be extruded
\item[polyline] is a list of (vertex,thickness) pairs, where vertex is a vertex of the polyline and
   thickness is a scaling factor (usually 1.0) representing how large the figure should be at that vertex.
\end{description}

\begin{code}
extrude :: Texture -> Integer -> Vector3D -> [Point3D] -> [(Point3D,Double)] -> PrimitiveModel
extrude tex subdivs up figure polyline =
  let points = map (\subdiv -> extrude_ subdiv subdivs up figure polyline) [0..subdivs]
      in frame tex $ transpose points

extrude_ :: Integer -> Integer -> Vector3D -> [Point3D] -> [(Point3D,Double)] -> [Point3D]
extrude_ 0 _ up figure polyline = extrude1Frame 0.0 up figure $ head $ doubles polyline
extrude_ subdiv subdivs up figure polyline | subdiv == subdivs = extrude1Frame 1.0 up figure $ last $ doubles polyline
extrude_ subdiv subdivs up figure polyline =
  let segments = doubles polyline
      distance_into_polyline = ((subdiv * genericLength segments) % subdivs)
      polyline_index = max 0 $ min (genericLength segments) $ floor distance_into_polyline
      segment = segments `genericIndex` polyline_index
      in extrude1Frame (fromRational distance_into_polyline - fromInteger polyline_index) up figure segment

extrude1Frame :: Double -> Vector3D -> [Point3D] -> ((Point3D,Double),(Point3D,Double)) -> [Point3D]
extrude1Frame fraction_of_segment up figure ((a,a_scale),(b,b_scale)) = 
  let to_point = vectorToFrom 
                     (RSAGL.Affine.translate (vectorScaleTo (fraction_of_segment * distanceBetween a b) $ vectorToFrom b a) a) 
                     origin_point_3d
      scale_factor = fraction_of_segment * b_scale + (1 - fraction_of_segment) * a_scale
      z_vector = vectorScaleTo scale_factor $ vectorToFrom b a
      x_vector = vectorScaleTo scale_factor $ crossProduct z_vector up
      y_vector = vectorScaleTo scale_factor $ crossProduct x_vector z_vector
      transformation_matrix = xyzMatrix x_vector y_vector z_vector
      in map (RSAGL.Affine.translate to_point) $ (map (RSAGL.Affine.transform transformation_matrix) figure :: [Point3D])     
\end{code}

\subsection{Modelling with surfaces of revolution}

sor constructs a surface of revolution.  In a SOR, a two dimensional frame is rotated around the Y axis.  This is the computer graphics equivalent of a potter's wheel.  The second parameter indicates the number of subdivisions.  The third parameter  is the two dimensional frame.

\begin{code}
sor :: Texture -> Integer -> [Point2D] -> PrimitiveModel
sor = deformedSor id
\end{code}

As a SOR, but apply a deformation function to all points in the SOR.

\begin{code}
deformedSor :: (Point3D -> Point3D) -> Texture -> Integer -> [Point2D] -> PrimitiveModel
deformedSor dfn tex subdivisions pts = RSAGL.PrimitiveModel.union $ deformedSorUnion dfn tex subdivisions pts

deformedSorUnion :: (Point3D -> Point3D) -> Texture -> Integer -> [Point2D] -> [PrimitiveModel]
deformedSorUnion _ _ subdivisions _ | subdivisions < 3 = error "sor: requires at least 3 subdivisions"
deformedSorUnion dfn tex subdivisions pts = 
    let points = map ((\m -> map (transformHomogenous m) pts) . rotationMatrix (Vector3D 0 1 0)) $ 
                     reverse $ angularIncrements subdivisions
        in frameUnion tex $ map (map dfn) points
\end{code}

frame contructs a surface from a sequence of polyline tails.  This is like wrapping a closed loop of cloth around a wire frame.
The result is always a union of longitudinal elements.

\begin{code}
frame :: Texture -> [[Point3D]] -> PrimitiveModel
frame t ps = RSAGL.PrimitiveModel.union $ frameUnion t ps

frameUnion :: Texture -> [[Point3D]] -> [PrimitiveModel]
frameUnion tex pts = 
     let normals_smooth_one_way = map ((quadStripToNormals).(uncurry zip)) $ loopedDoubles pts :: [[Vector3D]]
	 normals_smooth_both_ways = map (map vectorAverage . transpose) $ loopedConsecutives 2 normals_smooth_one_way
	 in map (Strip tex . uncurry zip) $ 
                loopedDoubles $ map (uncurry zip) $ 
                zip pts ((last normals_smooth_both_ways) : normals_smooth_both_ways)
\end{code}

\subsection{Unions of Models}

union takes a list of Models and combines them into a single Model.
decomposeModel takes a model and recursively decomposes it into its primitive components.

\begin{code}
union :: [PrimitiveModel] -> PrimitiveModel
union = Union

decomposeModel :: PrimitiveModel -> [PrimitiveModel]
decomposeModel (Union u) = concatMap decomposeModel u
decomposeModel m = [m]
\end{code}

\subsection{Transformations of models}

Transformations to a model can be performed in two ways.  Normally a model is transformed by adding the transformation
information to the model and then passing that information to OpenGL when the model is drawn.  However, the pretransform
function can apply an affine transformation to a model on a per-vertex basis.

\begin{code}
instance AffineTransformable PrimitiveModel where
    transform mat model = Transformation mat model

pretransform :: RSAGL.Matrix.Matrix Double -> PrimitiveModel -> PrimitiveModel
pretransform m (Transformation m' model) = pretransform (matrixMultiply m m') model
pretransform m (Triangle tex (p1,v1) (p2,v2) (p3,v3)) = 
    Triangle (transform m tex) (transform m p1,vectorNormalize $ transform m v1) (transform m p2,vectorNormalize $ transform m v2) (transform m p3,vectorNormalize $ transform m v3)
pretransform m (Strip tex pts) = 
    Strip (transform m tex) $ map (\((p1,v1),(p2,v2)) -> ((transform m p1,vectorNormalize $ transform m v1),(transform m p2,vectorNormalize $ transform m v2))) pts
pretransform m (Union models) = Union $ map (pretransform m) models
\end{code}

modelSize answers the greatest distance of any point from the origin of the PrimitiveModel.

\begin{code}
modelSize :: PrimitiveModel -> Double
modelSize (Triangle _ (p1,_) (p2,_) (p3,_)) = maximum $ map (distanceBetween origin_point_3d) [p1,p2,p3]
modelSize (Strip _ pts) = maximum $ map (distanceBetween origin_point_3d) $ concatMap (\((p1,_),(p2,_)) -> [p1,p2]) pts
modelSize (Union models) = maximum $ map modelSize models
modelSize transformedModel@(Transformation {}) = modelSize $ pretransform (identityMatrix 4) transformedModel
\end{code}

scaleModel forces a model to be the specified size, as defined by modelSize.

\begin{code}
scaleModel :: Double -> PrimitiveModel -> PrimitiveModel
scaleModel scalar model = scale' (scalar / modelSize model) model
\end{code}

\subsection{Rendering Models in OpenGL}

\begin{code}
toOpenGL :: PrimitiveModel -> IO ()
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

toVertex3 :: Point3D -> Vertex3 Double
toVertex3 (Point3D x y z) = Vertex3 x y z

toNormal3 :: Vector3D -> Normal3 Double
toNormal3 (Vector3D x y z) = Normal3 x y z

toColor4_rgb :: Material -> Color4 GLfloat
toColor4_rgb (Material { rgb=(RSAGL.PrimitiveModel.RGB r g b), alpha=a }) = 
    Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac $ fromMaybe 1 a)

toColor4_lum :: Material -> Color4 GLfloat
toColor4_lum (Material { lum=(Just (RSAGL.PrimitiveModel.RGB r g b))}) = Color4 (realToFrac r) (realToFrac g) (realToFrac b) 0
toColor4_lum _ = Color4 0 0 0 0

materialToOpenGL :: Material -> IO ()
materialToOpenGL c = 
    let shininess = realToFrac $ fromMaybe 0 (shine c)
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
\end{code}