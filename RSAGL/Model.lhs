\section{Haskell as a 3D Modelling Language: RSAGL.Model}

RSAGL.Model seeks to provide a complete set of high-level modelling primitives for OpenGL.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Model
    (Model,
     Modeling,
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
     torus,
     openCone,
     closedCone,
     openDisc,
     closedDisc,
     quadralateral,
     triangle,
     box,
     adaptive,
     fixed,
     twoSided,
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

import RSAGL.Auxiliary
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
import RSAGL.Affine
import RSAGL.Angle
import RSAGL.Color
import RSAGL.BoundingBox
import Data.List as List
import Data.Maybe
import Control.Monad.State hiding (get)
import Data.Monoid
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.Colors (lightModelTwoSide,Face(..))
import Graphics.Rendering.OpenGL.GL.StateVar as StateVar
import Graphics.Rendering.OpenGL.GL.Polygons
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
    ms_affine_transform :: Maybe AffineTransformation,
    ms_tesselation :: Maybe ModelTesselation,
    ms_tesselation_hint_complexity :: Integer,
    ms_two_sided :: Bool,
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
    ms_tesselation_hint_complexity = 1,
    ms_two_sided = False,
    ms_attributes = mempty }]

generalSurface :: (Monoid attr) => Either (Surface Point3D) (Surface (Point3D,Vector3D)) -> Modeling attr
generalSurface (Right pvs) = appendSurface $ uncurry SurfaceVertex3D <$> pvs
generalSurface (Left points) = appendSurface $ generateNormals points

generateNormals :: Surface Point3D -> Surface SurfaceVertex3D
generateNormals s = SurfaceVertex3D <$> s <*> fmap (vectorNormalize . uncurry crossProduct) (surfaceDerivative s)

tesselationHintComplexity :: (Monoid attr) => Integer -> Modeling attr
tesselationHintComplexity i = modify (map $ \m -> m { ms_tesselation_hint_complexity = i })

twoSided :: (Monoid attr) => Bool -> Modeling attr
twoSided two_sided = modify (map $ \m -> m { ms_two_sided = two_sided })

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
    transform mx m = model $ m >> affine (transform mx)

affine :: AffineTransformation -> Modeling attr
affine f = modify $ map (\x -> x { ms_affine_transform = Just $ (f .) $ fromMaybe id $ ms_affine_transform x })

deform :: (DeformationClass dc) => dc -> Modeling attr
deform dc = 
    do finishModeling
       case deformation dc of
                (Left f) -> modify (map $ \m -> m { ms_surface = generateNormals $ fmap f $ ms_surface m })
                (Right f) -> modify (map $ \m -> m { ms_surface = fmap (sv3df f) $ ms_surface m })
  where sv3df f sv3d = let SurfaceVertex3D p v = f sv3d in SurfaceVertex3D p (vectorNormalize v)

finishModeling :: Modeling attr
finishModeling = modify (map $ \m -> if isNothing (ms_affine_transform m) then m else finishAffine m)
    where finishAffine m = m { ms_surface = fmap (\(SurfaceVertex3D p v) -> SurfaceVertex3D p (vectorNormalize v)) $
                                                     transformation (fromJust $ ms_affine_transform m) (ms_surface m),
                               ms_affine_transform = Nothing }
\end{code}

\subsection{Coordinate System Alternatives for Parametric Surface Models}

\begin{code}
sphericalCoordinates :: ((Angle,Angle) -> a) -> Surface a
sphericalCoordinates f = surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,fromRadians $ ((pi/2) - v*pi) * (1 - 0.5^10))))

cylindricalCoordinates :: ((Angle,Double) -> a) -> Surface a
cylindricalCoordinates f = surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,v)))

toroidalCoordinates :: ((Angle,Angle) -> a) -> Surface a
toroidalCoordinates f = surface $ curry (f . (\(u,v) -> (fromRadians $ u*2*pi,fromRadians $ negate $ v*2*pi)))

planarCoordinates :: Point3D -> Vector3D -> ((Double,Double) -> (Double,Double)) -> Surface (Point3D,Vector3D)
planarCoordinates center up f = surface (curry $ g . f)
    where (u',v') = orthos up
          g (u,v) = (translate (vectorScale u u' `vectorAdd` vectorScale v v') center, up)
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

openDisc :: (Monoid attr) => Double -> Double -> Modeling attr
openDisc inner_radius outer_radius = model $ 
    do generalSurface $ Right $
        cylindricalCoordinates $ \(u,v) -> 
             (Point3D (lerp v (inner_radius,outer_radius) * cosine u)
                      0
                      (lerp v (inner_radius,outer_radius) * sine u),
              Vector3D 0 1 0)
       tesselationHintComplexity $ round $ (max outer_radius inner_radius / (abs $ outer_radius - inner_radius))

closedDisc :: (Monoid attr) => Point3D -> Vector3D -> Double -> Modeling attr
closedDisc center up radius = model $
    do generalSurface $ Right $ planarCoordinates center up $ \(u,v) ->
           let (u',v') = (2 * (u-0.5),2 * (v-0.5))
               k = min (recip $ abs u') (recip $ abs v')
               (u_square,v_square) = (k*u',k*v')
               x = (sqrt 2) / (sqrt $ u_square^2 + v_square^2)
               in if u' == 0 && v' == 0 then (0,0) else (radius*x*u',radius*x*v')

closedCone :: (Monoid attr) => (Point3D,Double) -> (Point3D,Double) -> Modeling attr
closedCone a b = model $
    do openCone a b
       closedDisc (fst a) (vectorToFrom (fst a) (fst b)) (snd a * (1 + recip (2^8)))
       closedDisc (fst b) (vectorToFrom (fst b) (fst a)) (snd b * (1 + recip (2^8)))

quadralateral :: (Monoid attr) => Point3D -> Point3D -> Point3D -> Point3D -> Modeling attr
quadralateral a b c d = model $ 
    do normal_vector <- return $ newell [a,b,c,d]
       generalSurface $ Right $ surface $ \u v -> (lerp v (lerp u (a,b), lerp u (d,c)),normal_vector)

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
          extraComplexity m = (1 + normalSurfaceArea (ms_surface m)) * 
                              (1 + fromInteger (ms_tesselation_hint_complexity m)) * 
                              (1 + fromInteger (materialComplexity $ ms_material m))
          normalSurfaceArea = estimateSurfaceArea sv3d_normal_ruler

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
layerToOpenGL tesselation layer = 
    (materialLayerToOpenGLWrapper layer) $ foldr (>>) (return ()) $ map (tesselatedElementToOpenGL toOpenGL) tesselation
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
    where opaques = filter isOpaque ms
          transparents = filter (not . isOpaque) ms
          isOpaque (IntermediateModeledSurface layers _) = isOpaqueLayer $ snd $ head layers
\end{code}

\subsubsection{Vertex Clouds and Bounding Boxes for IntermediateModels}

\begin{code}
intermediateModelToVertexCloud :: IntermediateModel -> [SurfaceVertex3D]
intermediateModelToVertexCloud (IntermediateModel ms) = concatMap intermediateModeledSurfaceToVertexCloud ms

instance Bound3D IntermediateModel where
    boundingBox (IntermediateModel ms) = boundingBox ms

intermediateModeledSurfaceToVertexCloud :: IntermediateModeledSurface -> [SurfaceVertex3D]
intermediateModeledSurfaceToVertexCloud (IntermediateModeledSurface layers _) = map strip $ tesselatedSurfaceToVertexCloud $ fst $ head layers
        where strip (SingleMaterialSurfaceVertex3D sv3d _) = sv3d

instance Bound3D IntermediateModeledSurface where
    boundingBox = boundingBox . intermediateModeledSurfaceToVertexCloud
\end{code}

\subsubsection{Rulers and Concavity Detection}

\begin{code}
sv3d_ruler :: SurfaceVertex3D -> SurfaceVertex3D -> Double
sv3d_ruler (SurfaceVertex3D p1 _) (SurfaceVertex3D p2 _) =
    distanceBetween p1 p2

sv3d_normal_ruler :: SurfaceVertex3D -> SurfaceVertex3D -> Double
sv3d_normal_ruler (SurfaceVertex3D _ v1) (SurfaceVertex3D _ v2) =
    distanceBetween v1 v2

msv3d_ruler :: MultiMaterialSurfaceVertex3D -> MultiMaterialSurfaceVertex3D -> Double
msv3d_ruler (MultiMaterialSurfaceVertex3D p1 _) (MultiMaterialSurfaceVertex3D p2 _) =
    sv3d_ruler p1 p2

instance ConcavityDetection MultiMaterialSurfaceVertex3D where
    toPoint3D (MultiMaterialSurfaceVertex3D (SurfaceVertex3D p _) _) = p
\end{code}

\subsubsection{Parallelism for IntermediateModels}

\begin{code}
instance NFData IntermediateModel where
    rnf (IntermediateModel ms) = rnf ms

parIntermediateModel :: Strategy IntermediateModel
parIntermediateModel (IntermediateModel ms) = seqList parIntermediateModeledSurface ms

instance NFData IntermediateModeledSurface where
    rnf (IntermediateModeledSurface layers two_sided) = rnf (layers,two_sided)

parIntermediateModeledSurface :: Strategy IntermediateModeledSurface
parIntermediateModeledSurface (IntermediateModeledSurface layers two_sided) = two_sided `seq` waitParList rnf layers

instance NFData SingleMaterialSurfaceVertex3D where
    rnf (SingleMaterialSurfaceVertex3D sv3d mv3d) = rnf (sv3d,mv3d)

instance NFData MaterialVertex3D where
    rnf (MaterialVertex3D cm b) = rnf (cm,b)
\end{code}