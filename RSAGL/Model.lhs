\section{Haskell as a 3D Modelling Language: RSAGL.Model}

RSAGL.Model seeks to provide a complete set of high-level modelling primitives for OpenGL.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Model
    (Model,
     Modeling,
     ModelingM,
     MaterialM,
     IntermediateModel,
     parIntermediateModel,
     generalSurface,
     extractModel,
     toIntermediateModel,
     intermediateModelToOpenGL,
     intermediateModelToVertexCloud,
     splitOpaques,
     modelingToOpenGL,
     sphere,
     skySphere,
     hemisphere,
     skyHemisphere,
     perspectiveSphere,
     torus,
     openCone,
     closedCone,
     openDisc,
     closedDisc,
     quadralateral,
     triangle,
     box,
     sor,
     tube,
     prism,
     adaptive,
     fixed,
     tesselationHintComplexity,
     twoSided,
     reverseOrientation,
     regenerateNormals,
     attribute,
     withAttribute,
     model,
     RGBFunction,RGBAFunction,
     material,pigment,specular,emissive,transparent,filtering,
     MonadAffine(..),
     turbulence,
     deform)
    where

import RSAGL.Curve
import RSAGL.CurveExtras
import RSAGL.Auxiliary
import Control.Applicative
import RSAGL.ApplicativeWrapper
import Data.Traversable
import RSAGL.Deformation
import RSAGL.Vector
import RSAGL.Material
import RSAGL.Tesselation
import RSAGL.Optimization
import RSAGL.Interpolation
import RSAGL.Affine
import RSAGL.CoordinateSystems
import RSAGL.Angle
import RSAGL.Color
import RSAGL.Extrusion
import RSAGL.BoundingBox
import Data.List as List
import Data.Maybe
import qualified Control.Monad.State as State
import Data.Monoid
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.Colors (lightModelTwoSide,Face(..))
import Graphics.Rendering.OpenGL.GL.StateVar as StateVar
import Graphics.Rendering.OpenGL.GL.Polygons
\end{code}

\subsection{Modeling Monad}

A ModeledSurface consists of several essential fields: \texttt{ms\_surface} is the geometric surface.

\texttt{ms\_material} defaults to invisible if no material is ever applied.  The functions \texttt{pigment}, \texttt{transparent}, \texttt{emissive}, and \texttt{specular} apply material properties to a surface.

Scope is controlled by \texttt{model} and \texttt{withAttribute}.  \texttt{model} creates a block of modeling operations that don't affect any surfaces outside of that block.  \texttt{withAttribute} restricts all operations to a subset of surfaces defined by \texttt{attribute}.

\texttt{ms\_tesselation} describes how the model will be tesselated into polygons before being sent to OpenGL.
By default, the \texttt{adaptive} model is used, which adapts to the contour and material of each surface.
\texttt{fixed} can be used to crudely force the tesselation of objects.

\begin{code}
type Model attr = [ModeledSurface attr]

data ModeledSurface attr = ModeledSurface {
    ms_surface :: Surface SurfaceVertex3D,
    ms_material :: Material,
    ms_affine_transform :: Maybe AffineTransformation,
    ms_tesselation :: Maybe ModelTesselation,
    ms_tesselation_hint_complexity :: Integer,
    ms_two_sided :: Bool,
    ms_attributes :: attr }

data ModelTesselation = Adaptive
                      | Fixed (Integer,Integer)

data Quasimaterial = forall a. Quasimaterial (ApplicativeWrapper ((->)SurfaceVertex3D) a) (MaterialSurface a -> Material)

newtype ModelingM attr a = ModelingM (State.State (Model attr) a) deriving (Monad)
newtype MaterialM attr a = MaterialM (State.State [Quasimaterial] a) deriving (Monad)
type Modeling attr = ModelingM attr ()

instance State.MonadState [ModeledSurface attr] (ModelingM attr) where
    get = ModelingM State.get
    put = ModelingM . State.put

instance State.MonadState [Quasimaterial] (MaterialM attr) where
    get = MaterialM State.get
    put = MaterialM . State.put

extractModel :: Modeling attr -> Model attr
extractModel (ModelingM m) = State.execState m []

appendSurface :: (Monoid attr) => Surface SurfaceVertex3D -> Modeling attr
appendSurface s = State.modify $ mappend $ [ModeledSurface {
    ms_surface = s,
    ms_material = mempty,
    ms_affine_transform = Nothing,
    ms_tesselation = Nothing,
    ms_tesselation_hint_complexity = 1,
    ms_two_sided = False,
    ms_attributes = mempty }]

generalSurface :: (Monoid attr) => Either (Surface Point3D) (Surface (Point3D,Vector3D)) -> Modeling attr
generalSurface (Right pvs) = appendSurface $ uncurry SurfaceVertex3D <$> pvs
generalSurface (Left points) = appendSurface $ surfaceNormals3D points

twoSided :: (Monoid attr) => Bool -> Modeling attr
twoSided two_sided = State.modify (map $ \m -> m { ms_two_sided = two_sided })

-- | Swap inside and outside surfaces and reverse normal vectors.  This shouldn't effect 'twoSided' surfaces in any visible way.
reverseOrientation :: (Monoid attr) => Modeling attr -> Modeling attr
reverseOrientation modelingA = model $
    do modelingA
       State.modify $ map $ \m -> m { ms_surface = transposeSurface $ ms_surface m }
       deform $ \(SurfaceVertex3D p v) -> SurfaceVertex3D p $ vectorScale (-1) v
\end{code}

\subsection{Tesselation Hints}

\begin{code}
tesselationHintComplexity :: (Monoid attr) => Integer -> Modeling attr
tesselationHintComplexity i = State.modify (map $ \m -> m { ms_tesselation_hint_complexity = i })

adaptive :: Modeling attr
adaptive = State.modify (map $ \m -> m { ms_tesselation = ms_tesselation m `State.mplus` (Just Adaptive) })

fixed :: (Integer,Integer) -> Modeling attr
fixed x = State.modify (map $ \m -> m { ms_tesselation = ms_tesselation m `State.mplus` (Just $ Fixed x) })
\end{code}

\texttt{regenerateNormals} is mostly used for debugging and strips and recomputes the normal vector data for
every surface that is in scope.

\begin{code}
regenerateNormals :: (Monoid attr) => Modeling attr
regenerateNormals = deform (id :: Point3D -> Point3D)
\end{code}

\subsection{Scoping Rules}

The \texttt{Modeling} monad has scoping rules that prevent nested modeling operations
from affecting unrelated surfaces.

\texttt{model} brackets which surfaces are considered in scope.  
\texttt{attribute} tags all surfaces that are in scope with a user attribute.
\texttt{withAttribute} filters which surfaces are considered in scope.

\begin{code}
model :: Modeling attr -> Modeling attr
model (ModelingM actions) = State.modify (State.execState actions [] ++)

attribute :: (Monoid attr) => attr -> Modeling attr
attribute attr = State.modify (map $ \m -> m { ms_attributes = attr `mappend` ms_attributes m })

withAttribute :: (attr -> Bool) -> Modeling attr -> Modeling attr
withAttribute f actions = withFilter (f . ms_attributes) actions

withFilter :: (ModeledSurface attr -> Bool) -> Modeling attr -> Modeling attr
withFilter f (ModelingM actions) = State.modify (\m -> State.execState actions (filter f m) ++ filter (not . f) m)
\end{code}

\subsection{Materials}

\begin{code}
class MonadMaterial m where
    material :: MaterialM attr () -> m attr ()

instance MonadMaterial ModelingM where
    material (MaterialM actions) = 
        do finishModeling
	   withFilter (materialIsEmpty . ms_material) $ mapM_ appendQuasimaterial $ State.execState actions []

instance MonadMaterial MaterialM where
    material (MaterialM actions) = State.modify (++ State.execState actions [])

appendQuasimaterial :: Quasimaterial -> ModelingM attr ()
appendQuasimaterial (Quasimaterial vertexwise_f material_constructor) | isPure vertexwise_f = State.modify (map $ \m ->
    m { ms_material = ms_material m `mappend` (material_constructor $ pure $ fromJust $ fromPure vertexwise_f) })
appendQuasimaterial (Quasimaterial vertexwise_f material_constructor) = State.modify (map $ \m ->
    m { ms_material = ms_material m `mappend` (material_constructor $ 
                          wrapApplicative $ fmap (toApplicative vertexwise_f) $ ms_surface m) })

type RGBFunction = ApplicativeWrapper ((->) SurfaceVertex3D) RGB
type RGBAFunction = ApplicativeWrapper ((->) SurfaceVertex3D) RGBA

pigment :: RGBFunction -> MaterialM attr ()
pigment rgbf = State.modify (++ [Quasimaterial rgbf diffuseLayer])

specular :: GLfloat -> RGBFunction -> MaterialM attr ()
specular shininess rgbf = State.modify (++ [Quasimaterial rgbf (flip specularLayer shininess)])

emissive :: RGBFunction -> MaterialM attr ()
emissive rgbf = State.modify (++ [Quasimaterial rgbf emissiveLayer])

transparent :: RGBAFunction -> MaterialM attr ()
transparent rgbaf = State.modify (++ [Quasimaterial rgbaf transparentLayer])

filtering :: RGBFunction -> MaterialM attr ()
filtering rgbf = State.modify (++ [Quasimaterial rgbf filteringLayer])
\end{code}

\subsection{Transformations of Surfaces and Materials}

\begin{code}
instance AffineTransformable (ModelingM attr ()) where
    transform mx m = model $ m >> affine (transform mx)

instance AffineTransformable (MaterialM attr ()) where
    transform mx m = material $ m >> affine (transform mx)

class MonadAffine m where
    affine :: AffineTransformation -> m ()

instance MonadAffine (ModelingM attr) where
    affine f = State.modify $ map (\x -> x { ms_affine_transform = Just $ (f .) $ fromMaybe id $ ms_affine_transform x })

instance MonadAffine (MaterialM attr) where
    affine f = turbulence (inverseTransformation f)

turbulence :: (SurfaceVertex3D -> SurfaceVertex3D) -> MaterialM attr ()
turbulence g = State.modify $ map (\(Quasimaterial f c) -> Quasimaterial 
        (either (wrapApplicative . (. g)) pure $ unwrapApplicative f) c)

deform :: (DeformationClass dc) => dc -> Modeling attr
deform dc = 
    do finishModeling
       case deformation dc of
                (Left f) -> State.modify (map $ \m -> m { ms_surface = surfaceNormals3D $ fmap f $ ms_surface m })
                (Right f) -> State.modify (map $ \m -> m { ms_surface = fmap (sv3df f) $ ms_surface m })
  where sv3df f sv3d = let SurfaceVertex3D p v = f sv3d in SurfaceVertex3D p (vectorNormalize v)

finishModeling :: Modeling attr
finishModeling = State.modify (map $ \m -> if isNothing (ms_affine_transform m) then m else finishAffine m)
    where finishAffine m = m { ms_surface = fmap (\(SurfaceVertex3D p v) -> SurfaceVertex3D p (vectorNormalize v)) $
                                                     transformation (fromJust $ ms_affine_transform m) (ms_surface m),
                               ms_affine_transform = Nothing }
\end{code}

\subsection{Simple Geometric Shapes}

\begin{code}
sphere :: (Monoid attr) => Point3D -> Double -> Modeling attr
sphere (Point3D x y z) radius = model $ do
    generalSurface $ Right $
        sphericalCoordinates $ (\(u,v) -> 
            let sinev = sine v
                cosinev = cosine v
                sineu = sine u
                cosineu = cosine u
                point = Point3D (x + radius * cosinev * cosineu)
                                (y + radius * sinev)
                                (z + radius * cosinev * sineu)
                vector = Vector3D (signum radius * cosinev * cosineu)
                                  (signum radius * sinev)
                                  (signum radius * cosinev * sineu)
                in (point,vector))

skySphere :: (Monoid attr) => Point3D -> Double -> Modeling attr
skySphere p r = sphere p (negate r)

hemisphere :: (Monoid attr) => Point3D -> Vector3D -> Double -> Modeling attr
hemisphere p v r = model $
    do generalSurface $ Right $ polarCoordinates $ \(a,d) -> let d_ = sqrt d
                                                                 x = cosine a*d_
                                                                 y = sqrt $ max 0 $ 1 - x*x - z*z
								 z = sine a*d_
                                                                 in (Point3D x y z,Vector3D x y z)
       affine $ translateToFrom p origin_point_3d . rotateToFrom v (Vector3D 0 1 0) . scale' r

skyHemisphere :: (Monoid attr) => Point3D -> Vector3D -> Double -> Modeling attr
skyHemisphere p v r = hemisphere p (vectorScale (-1) v) (negate r)

-- |
-- A 'perspectiveSphere' is rendered anticipating the point from which it is to be viewed.
-- Only the part of the surface of the sphere that would be visible from a vantage point is
-- rendered, and otherwise the sphere seems clipped.
--
-- This is the appropriate geometry to model the curvature of a planet from 200 kilometers altitude, for example.
perspectiveSphere :: (Monoid attr) => Point3D -> Double -> Point3D -> Modeling attr
perspectiveSphere center_point radius eye_point = model $
    do let d = distanceBetween center_point eye_point
       let  x = sqrt $ d**2 - radius**2
       let h = radius*x/d
       let d' = x*x/d
       closedDisc (lerpBetween (0,d',d) (eye_point,center_point)) (vectorToFrom eye_point center_point) h
       deform $ \(p :: Point3D) -> translate (vectorScaleTo radius $ vectorToFrom center_point p) center_point

torus :: (Monoid attr) => Double -> Double -> Modeling attr
torus major minor = model $
    do generalSurface $ Right $
        toroidalCoordinates $ \(u,v) ->
            (Point3D ((major + minor * cosine v) * cosine u)
                     (minor * sine v)
                     ((major + minor * cosine v) * sine u),
             Vector3D (cosine v * cosine u)
                      (minor * sine v)
                      (cosine v * sine u))
       tesselationHintComplexity $ round $ major / minor

openCone :: (Monoid attr) => (Point3D,Double) -> (Point3D,Double) -> Modeling attr
openCone (a,a_radius) (b,b_radius) = model $
    do generalSurface $ Right $
           cylindricalCoordinates $ \(u,v) ->
               let uv' = vectorScale (cosine u) u' `vectorAdd` vectorScale (sine u) v'
                   in (translate (vectorScale (lerp v (a_radius,b_radius)) uv') $ lerp v (a,b),
                       vectorNormalize $ vectorScale slope axis `vectorAdd` uv')
           where (u',v') = orthos axis
                 axis = vectorNormalize $ vectorToFrom b a
                 slope = (b_radius - a_radius) / distanceBetween a b

openDisc :: (Monoid attr) => Point3D -> Vector3D -> Double -> Double -> Modeling attr
openDisc p up 0 outer_radius = closedDisc p up outer_radius
openDisc p up inner_radius outer_radius = model $ 
    do generalSurface $ Right $
           cylindricalCoordinates $ \(u,v) -> 
               (Point3D (lerp v (inner_radius,outer_radius) * cosine u)
                        0
                        (lerp v (inner_radius,outer_radius) * sine u),
                Vector3D 0 1 0)
       tesselationHintComplexity $ round $ (max outer_radius inner_radius / (abs $ outer_radius - inner_radius))
       affine $ translateToFrom p origin_point_3d . rotateToFrom up (Vector3D 0 1 0)
 
closedDisc :: (Monoid attr) => Point3D -> Vector3D -> Double -> Modeling attr
closedDisc center up_vector radius = model $
    do generalSurface $ Right $ circularCoordinates (\(x,z) -> (Point3D x 0 z,Vector3D 0 1 0))
       affine $ translateToFrom center origin_point_3d . rotateToFrom up_vector (Vector3D 0 1 0) . scale' radius

closedCone :: (Monoid attr) => (Point3D,Double) -> (Point3D,Double) -> Modeling attr
closedCone a b = model $
    do openCone a b
       closedDisc (fst a) (vectorToFrom (fst a) (fst b)) (snd a * (1 + recip (2^8)))
       closedDisc (fst b) (vectorToFrom (fst b) (fst a)) (snd b * (1 + recip (2^8)))

quadralateral :: (Monoid attr) => Point3D -> Point3D -> Point3D -> Point3D -> Modeling attr
quadralateral a b c d = model $ 
    do let degenerate_message = error $ "quadralateral: " ++ show (a,b,c,d) ++ " seems to be degenerate."
       normal_vector <- return $ fromMaybe (degenerate_message) $ newell [a,b,c,d]
       generalSurface $ Right $ surface $ \u v -> (lerpClamped v (lerpClamped u (a,b), lerpClamped u (d,c)),normal_vector)

triangle :: (Monoid attr) => Point3D -> Point3D -> Point3D -> Modeling attr
triangle a b c | distanceBetween a b > distanceBetween b c = triangle c a b
triangle a b c | distanceBetween a c > distanceBetween b c = triangle b c a
triangle a b c = quadralateral a b (lerp 0.5 (b,c)) c

box :: (Monoid attr) => Point3D -> Point3D -> Modeling attr
box (Point3D x1 y1 z1) (Point3D x2 y2 z2) = model $
    do let [lx,hx] = sort [x1,x2]
       let [ly,hy] = sort [y1,y2]
       let [lz,hz] = sort [z1,z2]
       let u = minimum [hx-lx,hy-ly,hz-lz] / 2^8
       let (lx',ly',lz',hx',hy',hz') = (lx-u,ly-u,lz-u,hx+u,hy+u,hz+u)
       quadralateral (Point3D lx' ly' lz) (Point3D lx' hy' lz) (Point3D hx' hy' lz) (Point3D hx' ly' lz)  -- near
       quadralateral (Point3D lx' ly' hz) (Point3D hx' ly' hz) (Point3D hx' hy' hz) (Point3D lx' hy' hz)  -- far
       quadralateral (Point3D lx' ly lz') (Point3D hx' ly lz') (Point3D hx' ly hz') (Point3D lx' ly hz')  -- bottom
       quadralateral (Point3D lx' hy lz') (Point3D lx' hy hz') (Point3D hx' hy hz') (Point3D hx' hy lz')  -- top
       quadralateral (Point3D lx ly' lz') (Point3D lx ly' hz') (Point3D lx hy' hz') (Point3D lx hy' lz')  -- left
       quadralateral (Point3D hx ly' lz') (Point3D hx hy' lz') (Point3D hx hy' hz') (Point3D hx ly' hz')  -- right

sor :: (Monoid attr) => Curve Point3D -> Modeling attr
sor c = model $ generalSurface $ Left $ clampV $ transposeSurface $ wrapSurface $ curve (flip rotateY c . fromRotations)

tube :: (Monoid attr) => Curve (Double,Point3D) -> Modeling attr
tube c | radius <- fmap fst c 
       , spine <- fmap snd c = 
    model $ generalSurface $ Left $ clampV $ extrudeTube radius spine

prism :: (Monoid attr) => Vector3D -> (Point3D,Double) -> (Point3D,Double) -> Curve Point3D -> Modeling attr
prism upish ara brb c = model $ generalSurface $ Left $ clampV $ extrudePrism upish ara brb c
\end{code}

\subsection{Rendering Models to OpenGL}

\begin{code}
data IntermediateModel = IntermediateModel [IntermediateModeledSurface]
data IntermediateModeledSurface = IntermediateModeledSurface [(TesselatedSurface SingleMaterialSurfaceVertex3D,MaterialLayer)] Bool
data SingleMaterialSurfaceVertex3D = SingleMaterialSurfaceVertex3D SurfaceVertex3D MaterialVertex3D
data MultiMaterialSurfaceVertex3D = MultiMaterialSurfaceVertex3D SurfaceVertex3D [MaterialVertex3D]
data MaterialVertex3D = MaterialVertex3D RGBA Bool

intermediateModelToOpenGL :: IntermediateModel -> IO ()
intermediateModelToOpenGL (IntermediateModel ms) = mapM_ intermediateModeledSurfaceToOpenGL ms

modelingToOpenGL :: Integer -> Modeling attr -> IO ()
modelingToOpenGL n modeling = intermediateModelToOpenGL $ toIntermediateModel n modeling

toIntermediateModel :: Integer -> Modeling attr -> IntermediateModel
toIntermediateModel n modeling = IntermediateModel $ zipWith intermediateModeledSurface complexities ms
    where complexities = allocateComplexity sv3d_ruler (map (\m -> (ms_surface m,extraComplexity m)) ms) n
          ms = extractModel (modeling >> finishModeling)
          extraComplexity m = (1 + fromInteger (ms_tesselation_hint_complexity m)) * 
                              (1 + fromInteger (materialComplexity $ ms_material m))

intermediateModeledSurfaceToOpenGL :: IntermediateModeledSurface -> IO ()
intermediateModeledSurfaceToOpenGL (IntermediateModeledSurface layers two_sided) = 
    do lmts <- get lightModelTwoSide
       cf <- get cullFace
       lightModelTwoSide $= (if two_sided then Enabled else Disabled)
       cullFace $= (if two_sided then Nothing else Just Back)
       foldr (>>) (return ()) $ map (uncurry layerToOpenGL) layers
       lightModelTwoSide $= lmts
       cullFace $= cf

intermediateModeledSurface :: Integer -> ModeledSurface attr -> IntermediateModeledSurface
intermediateModeledSurface n m = IntermediateModeledSurface (zip (selectLayers (genericLength layers) tesselation) layers)
                                                            (ms_two_sided m)
    where layers = toLayers $ ms_material m
          color_material_layers :: [Surface RGBA]
          color_material_layers = map (toApplicative . materialLayerSurface) layers
          relevance_layers :: [Surface Bool]
          relevance_layers = map (toApplicative . materialLayerRelevant) layers
          the_surface = zipSurface (MultiMaterialSurfaceVertex3D) (ms_surface m) $ 
                                  sequenceA $ zipWith (zipSurface MaterialVertex3D) color_material_layers relevance_layers
          tesselation = case fromMaybe Adaptive $ ms_tesselation m of
                             Adaptive -> optimizeSurface msv3d_ruler the_surface (n `div` genericLength layers)
                             Fixed uv -> tesselateSurface the_surface uv

selectLayers :: Integer -> TesselatedSurface MultiMaterialSurfaceVertex3D -> [TesselatedSurface SingleMaterialSurfaceVertex3D]
selectLayers n layered = map (\k -> map (fmap (\(MultiMaterialSurfaceVertex3D sv3d mv3ds) -> 
                                                 SingleMaterialSurfaceVertex3D sv3d (mv3ds `genericIndex` k))) layered) [0..(n-1)]

layerToOpenGL :: TesselatedSurface SingleMaterialSurfaceVertex3D -> MaterialLayer -> IO ()
layerToOpenGL tesselation layer = materialLayerToOpenGLWrapper layer (mapM_ (tesselatedElementToOpenGL toOpenGL) tesselation)
        where vertexToOpenGLWithMaterialColor (SingleMaterialSurfaceVertex3D 
                                                  (SurfaceVertex3D (Point3D px py pz) (Vector3D vx vy vz))
                                                  (MaterialVertex3D color_material _)) =
                  do rgbaToOpenGL color_material
                     normal $ Normal3 vx vy vz
                     vertex $ Vertex3 px py pz
              vertexToOpenGL (SingleMaterialSurfaceVertex3D (SurfaceVertex3D (Point3D px py pz) (Vector3D vx vy vz)) _) =
                  do normal $ Normal3 vx vy vz
                     vertex $ Vertex3 px py pz
              toOpenGL = if isPure $ materialLayerSurface layer then vertexToOpenGL else vertexToOpenGLWithMaterialColor

\end{code}

\subsubsection{Seperating Opaque and Transparent Surfaces}

\texttt{splitOpaques} breaks an \texttt{IntermediateModel} into a pair containing the completely opaque surfaces of the model and a list
of transparent \texttt{IntermediateModel}s.

\begin{code}
splitOpaques :: IntermediateModel -> (IntermediateModel,[IntermediateModel])
splitOpaques (IntermediateModel ms) = (IntermediateModel opaques,map (\x -> IntermediateModel [x]) transparents)
    where opaques = filter isOpaque surfaces
          transparents = filter (not . isOpaque) surfaces
          isOpaque (IntermediateModeledSurface layers _) = any (isOpaqueLayer . snd) layers
	  notEmpty (IntermediateModeledSurface layers _) = not $ null layers
	  surfaces = filter notEmpty ms
\end{code}

\subsubsection{Vertex Clouds and Bounding Boxes for IntermediateModels}

\begin{code}
intermediateModelToVertexCloud :: IntermediateModel -> [SurfaceVertex3D]
intermediateModelToVertexCloud (IntermediateModel ms) = concatMap intermediateModeledSurfaceToVertexCloud ms

instance Bound3D IntermediateModel where
    boundingBox (IntermediateModel ms) = boundingBox ms

intermediateModeledSurfaceToVertexCloud :: IntermediateModeledSurface -> [SurfaceVertex3D]
intermediateModeledSurfaceToVertexCloud (IntermediateModeledSurface layers _) = 
    fromMaybe [] $ fmap (map strip . tesselatedSurfaceToVertexCloud . fst) $ listToMaybe layers
        where strip (SingleMaterialSurfaceVertex3D sv3d _) = sv3d

instance Bound3D IntermediateModeledSurface where
    boundingBox = boundingBox . intermediateModeledSurfaceToVertexCloud
\end{code}

\subsubsection{Rulers}

\begin{code}
sv3d_ruler :: SurfaceVertex3D -> SurfaceVertex3D -> Double
sv3d_ruler a b = sv3d_distance_ruler a b * (1.0 + sv3d_normal_ruler a b)

sv3d_distance_ruler :: SurfaceVertex3D -> SurfaceVertex3D -> Double
sv3d_distance_ruler (SurfaceVertex3D p1 _) (SurfaceVertex3D p2 _) =
    distanceBetween p1 p2

sv3d_normal_ruler :: SurfaceVertex3D -> SurfaceVertex3D -> Double
sv3d_normal_ruler (SurfaceVertex3D _ v1) (SurfaceVertex3D _ v2) =
    abs $ (1-) $ dotProduct v1 v2

msv3d_ruler :: MultiMaterialSurfaceVertex3D -> MultiMaterialSurfaceVertex3D -> Double
msv3d_ruler (MultiMaterialSurfaceVertex3D p1 _) (MultiMaterialSurfaceVertex3D p2 _) =
    sv3d_ruler p1 p2
\end{code}

\subsubsection{Parallelism for IntermediateModels}

\begin{code}
instance NFData IntermediateModel where
    rnf (IntermediateModel ms) = rnf ms

parIntermediateModel :: Strategy IntermediateModel
parIntermediateModel (IntermediateModel ms) = waitParList parIntermediateModeledSurface ms

instance NFData IntermediateModeledSurface where
    rnf (IntermediateModeledSurface layers two_sided) = rnf (layers,two_sided)

parIntermediateModeledSurface :: Strategy IntermediateModeledSurface
parIntermediateModeledSurface (IntermediateModeledSurface layers _) = waitParList rnf layers

instance NFData SingleMaterialSurfaceVertex3D where
    rnf (SingleMaterialSurfaceVertex3D sv3d mv3d) = rnf (sv3d,mv3d)

instance NFData MaterialVertex3D where
    rnf (MaterialVertex3D cm b) = rnf (cm,b)
\end{code}
