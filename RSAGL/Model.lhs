\section{Haskell as a 3D Modelling Language: RSAGL.Model}

RSAGL.Model seeks to provide a complete set of high-level modelling primitives for OpenGL.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Model
    (Model,
     Modeling,
     IntermediateModel,
     generalSurface,
     extractModel,
     toIntermediateModel,
     intermediateModelToOpenGL,
     modelingToOpenGL,
     sphere,
     disc,
     adaptive,
     fixed,
     attribute,
     withAttribute,
     model,
     RGBFunction,RGBAFunction,
     pigment,specular,emissive,transparent,
     affine,
     deform,
     sphericalCoordinates,
     cylindricalCoordinates)
    where

import Control.Applicative
import RSAGL.ApplicativeWrapper
import Data.Traversable
import RSAGL.Deformation
import RSAGL.Vector
import RSAGL.Surface
import RSAGL.Material
import RSAGL.Tesselation
import RSAGL.Optimization
import RSAGL.Interpolation
import RSAGL.Matrix
import RSAGL.Affine
import RSAGL.Angle
import Data.List as List
import Data.Maybe
import Control.Monad.State
import Data.Monoid
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.BasicTypes
\end{code}

\subsection{Modeling Primitives}

A ModeledSurface consists of several essential fields: \texttt{ms_surface} is the geometric surface.  .

\texttt{ms_material} defaults to invisible if no material is ever applied.  The functions \texttt{pigment}, \texttt{transparent}, \texttt{emissive}, and \texttt{specular} apply material properties to a surface.

Scope is controlled by \texttt{model} and \texttt{withAttribute}.  \texttt{model} creates a block of modeling operations that don't affect any surfaces outside of that block.  \texttt{withAttribute} restricts all operations to a subset of surfaces defined by \texttt{attribute}.

\texttt{ms_tesselation} describes how the model will be tesselated into polygons before being sent to OpenGL.
By default, the \texttt{adaptive} model is used, which adapts to the contour and material of each surface.
\texttt{fixed} can be used to crudely force the tesselation of objects.

\begin{code}
type Model attr = [ModeledSurface attr]

data ModeledSurface attr = ModeledSurface {
    ms_surface :: Surface SurfaceVertex3D,
    ms_material :: Material,
    ms_affine_transform :: Maybe Matrix,
    ms_tesselation :: Maybe ModelTesselation,
    ms_tesselation_hint_complexity :: Integer,
    ms_attributes :: attr }

data ModelTesselation = Adaptive
                      | Fixed (Integer,Integer)

type Modeling attr = State (Model attr) ()

extractModel :: Modeling attr -> Model attr
extractModel m = execState m []

appendSurface :: (Monoid attr) => Surface SurfaceVertex3D -> Modeling attr
appendSurface s = modify $ mappend $ [ModeledSurface {
    ms_surface = s,
    ms_material = mempty,
    ms_affine_transform = Nothing,
    ms_tesselation = Nothing,
    ms_tesselation_hint_complexity = 0,
    ms_attributes = mempty }]

generalSurface :: (Monoid attr) => Either (Surface Point3D) (Surface (Point3D,Vector3D)) -> Modeling attr
generalSurface (Right pvs) = appendSurface $ uncurry SurfaceVertex3D <$> pvs <*> uv_identity 
generalSurface (Left points) = appendSurface $ generateNormals points

generateNormals :: Surface Point3D -> Surface SurfaceVertex3D
generateNormals s = SurfaceVertex3D <$> s <*> fmap (vectorNormalize . uncurry crossProduct) (surfaceDerivative s) <*> uv_identity

tesselationHintComplexity :: (Monoid attr) => Integer -> Modeling attr
tesselationHintComplexity i = modify (map $ \m -> m { ms_tesselation_hint_complexity = i })

model :: Modeling attr -> Modeling attr
model actions = modify (execState actions [] ++)

attribute :: (Monoid attr) => attr -> Modeling attr
attribute attr = modify (map $ \m -> m { ms_attributes = attr `mappend` ms_attributes m })

withAttribute :: (attr -> Bool) -> Modeling attr -> Modeling attr
withAttribute f actions = modify (\m -> execState actions (goods m) ++ bads m)
    where goods = filter (f . ms_attributes)
          bads = filter (not . f . ms_attributes)

material :: (ApplicativeWrapper ((->)SurfaceVertex3D) a) -> (MaterialSurface a -> Material) -> Modeling attr
material vertexwise_f material_constructor | isPure vertexwise_f = modify (map $ \m ->
    m { ms_material = ms_material m `mappend` (material_constructor $ pure $ fromJust $ fromPure vertexwise_f) })
material vertexwise_f material_constructor = modify (map $ \m ->
    m { ms_material = ms_material m `mappend` (material_constructor $ 
                          wrapApplicative $ fmap (toApplicative vertexwise_f) $ ms_surface m) })

type RGBFunction = ApplicativeWrapper ((->) SurfaceVertex3D) RGB
type RGBAFunction = ApplicativeWrapper ((->) SurfaceVertex3D) RGBA

pigment :: RGBFunction -> Modeling attr
pigment rgbf = material rgbf diffuseLayer

specular :: GLfloat -> RGBFunction -> Modeling attr
specular shininess rgbf = material rgbf (flip specularLayer shininess)

emissive :: RGBFunction -> Modeling attr
emissive rgbf = material rgbf emissiveLayer

transparent :: RGBAFunction -> Modeling attr
transparent rgbaf = material rgbaf transparentLayer

adaptive :: Modeling attr
adaptive = modify (map $ \m -> m { ms_tesselation = ms_tesselation m `mplus` (Just Adaptive) })

fixed :: (Integer,Integer) -> Modeling attr
fixed x = modify (map $ \m -> m { ms_tesselation = ms_tesselation m `mplus` (Just $ Fixed x) })

instance AffineTransformable (Modeling attr) where
    transform mx m = m >> affine (transform mx)

affine :: (forall a. (AffineTransformable a) => a -> a) -> Modeling attr
affine f = modify $ map (\x -> x { ms_affine_transform = Just $ f $ fromMaybe (identityMatrix 4) $ ms_affine_transform x })

deform :: (DeformationClass dc) => dc -> Modeling attr
deform dc = 
    do finishModeling
       case deformation dc of
                (Left f) -> modify (map $ \m -> m { ms_surface = generateNormals $ fmap f $ ms_surface m })
                (Right f) -> modify (map $ \m -> m { ms_surface = fmap (sv3df f) $ ms_surface m })
  where sv3df f (sv3d@(SurfaceVertex3D _ _ uv)) = let (p,v) = f sv3d in SurfaceVertex3D p (vectorNormalize v) uv
\end{code}

\subsection{Coordinate System Alternatives for Parametric Surface Models}

\begin{code}
sphericalCoordinates :: ((Angle,Angle) -> a) -> Surface a
sphericalCoordinates f = surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,fromRadians $ (v*pi - (pi/2)) * (1 - 0.5^10))))

cylindricalCoordinates :: ((Angle,Double) -> a) -> Surface a
cylindricalCoordinates f = surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,v)))
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
                vector = Vector3D (cosinev * cosineu)
                                  (sinev)
                                  (cosinev * sineu)
                in (point,vector))
    tesselationHintComplexity 1

disc :: (Monoid attr) => Double -> Double -> Modeling attr
disc inner_radius outer_radius = model $ 
    do generalSurface $ Right $
        cylindricalCoordinates $ (\(u,v) -> 
             (Point3D (lerp v (outer_radius,inner_radius) * cosine u)
                      0
                      (lerp v (outer_radius,inner_radius) * sine u),
              up))
       tesselationHintComplexity $ round $ (max outer_radius inner_radius / (abs $ outer_radius - inner_radius)) ^ 2
           where up = Vector3D 0 1 0
\end{code}

\subsection{Rendering Models to OpenGL}

\begin{code}
finishModeling :: Modeling attr
finishModeling = modify (map $ \m -> if isNothing (ms_affine_transform m) then m else finishAffine m)
    where finishAffine m = m { ms_surface = fmap (\(SurfaceVertex3D p v uv) -> SurfaceVertex3D p (vectorNormalize v) uv) $
                                                     transform (fromJust $ ms_affine_transform m) (ms_surface m),
                               ms_affine_transform = Nothing }

data IntermediateModel = IntermediateModel [IntermediateModeledSurface]

data IntermediateModeledSurface = IntermediateModeledSurface [TesselatedSurface SingleMaterialSurfaceVertex3D] [MaterialLayer]

data SingleMaterialSurfaceVertex3D = SingleMaterialSurfaceVertex3D SurfaceVertex3D MaterialVertex3D

data MultiMaterialSurfaceVertex3D = MultiMaterialSurfaceVertex3D SurfaceVertex3D [MaterialVertex3D]

data MaterialVertex3D = MaterialVertex3D (IO ()) Bool

intermediateModelToOpenGL :: IntermediateModel -> IO ()
intermediateModelToOpenGL (IntermediateModel ms) = mapM_ intermediateModeledSurfaceToOpenGL ms

modelingToOpenGL :: Integer -> Modeling attr -> IO ()
modelingToOpenGL n modeling = intermediateModelToOpenGL $ toIntermediateModel n modeling

toIntermediateModel :: Integer -> Modeling attr -> IntermediateModel
toIntermediateModel n modeling = IntermediateModel $ map (\(m,c) -> intermediateModeledSurface c m) complexity_map
    where complexity_map = zip ms $ allocateComplexity sv3d_ruler (map (\m -> (ms_surface m,extraComplexity m)) ms) n
          ms = extractModel (modeling >> finishModeling)
          extraComplexity m = normalSurfaceArea (ms_surface m) * fromInteger (ms_tesselation_hint_complexity m * materialComplexity (ms_material m))
          normalSurfaceArea = estimateSurfaceArea sv3d_normal_ruler 

intermediateModeledSurfaceToOpenGL :: IntermediateModeledSurface -> IO ()
intermediateModeledSurfaceToOpenGL (IntermediateModeledSurface tesselations layers) = 
    foldr (>>) (return ()) $ zipWith layerToOpenGL tesselations layers

intermediateModeledSurface :: Integer -> ModeledSurface attr -> IntermediateModeledSurface
intermediateModeledSurface n m = IntermediateModeledSurface (selectLayers (genericLength layers) tesselation) layers
    where layers = toLayers $ ms_material m
          opengl_material_layers :: [Surface (IO ())]
          opengl_material_layers = map (either id (const $ pure $ return ()) . unwrapApplicative . materialLayerToOpenGL) layers
          relevance_layers :: [Surface Bool]
          relevance_layers = map (toApplicative . materialLayerRelevant) layers
          the_surface = zipSurface (MultiMaterialSurfaceVertex3D) (ms_surface m) $ 
                                  sequenceA $ zipWith (zipSurface MaterialVertex3D) opengl_material_layers relevance_layers
          tesselation = case fromMaybe Adaptive $ ms_tesselation m of
                             Adaptive -> optimizeSurface msv3d_ruler the_surface (n `div` genericLength layers)
                             Fixed uv -> tesselateSurface the_surface uv

selectLayers :: Integer -> TesselatedSurface MultiMaterialSurfaceVertex3D -> [TesselatedSurface SingleMaterialSurfaceVertex3D]
selectLayers n layered = map (\k -> map (fmap (\(MultiMaterialSurfaceVertex3D sv3d mv3ds) -> SingleMaterialSurfaceVertex3D sv3d (mv3ds `genericIndex` k))) layered) [0..n]

layerToOpenGL :: TesselatedSurface SingleMaterialSurfaceVertex3D -> MaterialLayer -> IO ()
layerToOpenGL tesselation layer = 
    (materialLayerToOpenGLWrapper layer) $ foldr (>>) (return ()) $ map (tesselatedElementToOpenGL vertexToOpenGL) tesselation
        where vertexToOpenGL (SingleMaterialSurfaceVertex3D 
                                    (SurfaceVertex3D (Point3D px py pz) (Vector3D vx vy vz) _) 
                                    (MaterialVertex3D material_io_action _)) =
                  do material_io_action
                     normal $ Normal3 vx vy vz
                     vertex $ Vertex3 px py pz

sv3d_ruler :: SurfaceVertex3D -> SurfaceVertex3D -> Double
sv3d_ruler (SurfaceVertex3D p1 _ _) (SurfaceVertex3D p2 _ _) =
    distanceBetween p1 p2

sv3d_normal_ruler :: SurfaceVertex3D -> SurfaceVertex3D -> Double
sv3d_normal_ruler (SurfaceVertex3D _ v1 _) (SurfaceVertex3D _ v2 _) =
    distanceBetween v1 v2

msv3d_ruler :: MultiMaterialSurfaceVertex3D -> MultiMaterialSurfaceVertex3D -> Double
msv3d_ruler (MultiMaterialSurfaceVertex3D p1 _) (MultiMaterialSurfaceVertex3D p2 _) =
    sv3d_ruler p1 p2

instance ConcavityDetection MultiMaterialSurfaceVertex3D where
    toPoint3D (MultiMaterialSurfaceVertex3D (SurfaceVertex3D p _ _) _) = p
\end{code}
