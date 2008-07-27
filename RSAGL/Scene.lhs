 cs\section{Scenes and Animation}

A \texttt{Scene} is a complete description of an image to be rendered, consisting of a camera position, light sources, and models.

\begin{code}

{-# OPTIONS_GHC -farrows #-}

module RSAGL.Scene
    (Scene,
     Camera(..),
     infiniteCameraOf,
     LightSource(..),
     skylight,
     infiniteLightSourceOf,
     SceneObject,
     SceneLayer,
     ScenicAccumulator(..),
     SceneAccumulator,
     null_scene_accumulator,
     sceneObject,
     cameraRelativeSceneObject,
     lightSource,
     accumulateSceneM,
     accumulateSceneA,
     assembleScene,
     sceneToOpenGL,
     stdSceneLayers,
     std_scene_layer_hud,
     std_scene_layer_cockpit,
     std_scene_layer_local,
     std_scene_layer_infinite,
     defaultLightSourceLayerTransform)
    where

import Data.Ord
import RSAGL.BoundingBox
import RSAGL.Vector
import RSAGL.Affine as Affine
import RSAGL.Angle as Angle
import RSAGL.Model
import RSAGL.CoordinateSystems
import Data.List
import Control.Monad.State as State
import Control.Arrow
import Control.Arrow.Operations
import RSAGL.Color as Color
import Graphics.UI.GLUT as GLUT
import Data.Maybe
import RSAGL.WrappedAffine
import RSAGL.Orthagonal
import qualified Data.Map as Map
import qualified Data.Set as Set
\end{code}

\subsection{Cameras}

\begin{code}
data Camera =
    PerspectiveCamera { camera_position, camera_lookat :: Point3D, 
                        camera_up :: Vector3D,
                        camera_fov :: Angle.Angle }

instance AffineTransformable Camera where
    transform m (pc@(PerspectiveCamera {})) = 
        pc { camera_position = transform m $ camera_position pc,
             camera_lookat = transform m $ camera_lookat pc,
             camera_up = transform m $ camera_up pc }

cameraToOpenGL :: Double -> (Double,Double) -> Camera -> IO ()
cameraToOpenGL aspect_ratio (near,far)
        (PerspectiveCamera { camera_position = (Point3D px py pz),
                             camera_lookat = (Point3D lx ly lz),
                             camera_up = (Vector3D ux uy uz),
                             camera_fov = fov }) =
    do matrixMode $= Projection
       loadIdentity
       perspective (toDegrees fov)
                   aspect_ratio
                   near
                   far
       matrixMode $= Modelview 0
       lookAt (Vertex3 px py pz) (Vertex3 lx ly lz) (Vector3 ux uy uz)

infiniteCameraOf :: Camera -> Camera
infiniteCameraOf pc = translateToFrom origin_point_3d (camera_position pc) pc

cameraOrientation :: (AffineTransformable a) => Camera -> a -> a
cameraOrientation c = modelLookAt (camera_position c) (forward $ Left $ camera_lookat c) 
                                                      (up $ Right $ camera_up c)

cameraLookAt :: (AffineTransformable a) => Camera -> a -> a
cameraLookAt = inverseTransformation . cameraOrientation 
\end{code}

\subsection{Light Sources}

\begin{code}
data LightSource =
      DirectionalLight { lightsource_direction :: Vector3D,
                         lightsource_color :: Color.RGB,
                         lightsource_ambient :: Color.RGB }
    | PointLight { lightsource_position :: Point3D,
                   lightsource_radius :: Distance,
                   lightsource_color :: Color.RGB,
                   lightsource_ambient :: Color.RGB }
    | NoLight

skylight :: Vector3D -> Color.RGB -> LightSource
skylight v c = DirectionalLight {
    lightsource_direction = v,
    lightsource_color = scaleRGB 0.7208681020859709 c,
    lightsource_ambient = scaleRGB 0.27913189791402915 c }

isNoLight :: LightSource -> Bool
isNoLight NoLight = True
isNoLight _ = False

infiniteLightSourceOf :: LightSource -> LightSource
infiniteLightSourceOf NoLight = NoLight
infiniteLightSourceOf (d@(DirectionalLight {})) = d
infiniteLightSourceOf (p@PointLight {}) = DirectionalLight {
    lightsource_direction = vectorToFrom origin_point_3d $ lightsource_position p,
    lightsource_color = scaleRGB scale_factor $ lightsource_color p,
    lightsource_ambient = scaleRGB scale_factor $ lightsource_ambient p }
        where scale_factor = realToFrac $ (distance $ lightsource_radius p) / (distanceBetweenSquared origin_point_3d (lightsource_position p))

instance AffineTransformable LightSource where
    transform _ NoLight = NoLight
    transform m (dl@(DirectionalLight {})) = dl { lightsource_direction = transform m $ lightsource_direction dl }
    transform m (pl@(PointLight {})) = pl {
        lightsource_position = transform m $ lightsource_position pl,
        lightsource_radius = transform m $ lightsource_radius pl }

setLightSources :: [LightSource] -> IO ()
setLightSources lss =
    do max_lights <- GLUT.get maxLights
       mapM_ setLightSource $ genericTake max_lights $ zip (map Light [0..]) (lss ++ repeat NoLight)

setLightSource :: (Light,LightSource) -> IO ()
setLightSource (l,NoLight) = light l $= Disabled
setLightSource (l,dl@DirectionalLight { lightsource_color = Color.RGB cr cg cb,
                                        lightsource_ambient = Color.RGB ar ag ab }) =
    do let Vector3D vx vy vz = vectorNormalize $ lightsource_direction dl
       light l $= Enabled
       ambient l $= (Color4 ar ag ab 1.0 :: Color4 Float)
       GLUT.specular l $= (Color4 cr cg cb 1.0 :: Color4 Float)
       diffuse l $= (Color4 cr cg cb 1.0 :: Color4 Float)
       position l $= (Vertex4 (realToFrac vx) (realToFrac vy) (realToFrac vz) 0 :: Vertex4 Float)
       attenuation l $= (1,0,0)
setLightSource (l,pl@(PointLight { lightsource_position = (Point3D px py pz),
                                   lightsource_color = Color.RGB cr cg cb,
                                   lightsource_ambient = Color.RGB ar ag ab })) =
    do light l $= Enabled
       ambient l $= (Color4 ar ag ab 1.0 :: Color4 Float)
       GLUT.specular l $= (Color4 cr cg cb 1.0 :: Color4 Float)
       diffuse l $= (Color4 cr cg cb 1.0 :: Color4 Float)
       position l $= (Vertex4 (realToFrac px) (realToFrac py) (realToFrac pz) 1 :: Vertex4 Float)
       attenuation l $= (0.01,0,recip $ realToFrac $ distanceSquared $ lightsource_radius pl)
\end{code}

\subsection{Scene Construction}

A \texttt{Scene} supports local and infinite scene layers.  The camera moves through the local scene layer, but the infinite scene layer is fixed.  Objects in the infinite scene layer never occlude objects in the local layer.  All light sources in the infinite scene layer are rendered as directional light sources in the local scene layer.  Local light sources are not rendered at all in the infinite layer.

Celestial objects such as the moon and sun, as well as the sky sphere, belong in the infinite subscene.  Distant clouds or mountains may also belong in the infinite layer.

\begin{code}
data SceneObject = 
    LightSource LightSource
  | Model (Camera -> IO (WrappedAffine IntermediateModel))

instance AffineTransformable SceneObject where
    transform m (LightSource ls) = LightSource $ transform m ls
    transform m (Model imodel) = Model $ \c -> liftM (transform m) (imodel c)

type SceneLayer = Integer

data SceneAccumulator = SceneAccumulator {
    sceneaccum_objs :: [(SceneLayer,SceneObject)],
    sceneaccum_coordinate_system :: CoordinateSystem }

instance CoordinateSystemClass SceneAccumulator where
    getCoordinateSystem = sceneaccum_coordinate_system
    storeCoordinateSystem cs sceneaccum = sceneaccum { sceneaccum_coordinate_system = cs }

class (CoordinateSystemClass a) => ScenicAccumulator a where
    accumulateScene :: SceneLayer -> SceneObject -> a -> a

instance ScenicAccumulator SceneAccumulator where
    accumulateScene slayer scobj sceneaccum = sceneaccum { 
        sceneaccum_objs = (slayer,migrateToFrom (sceneaccum_coordinate_system sceneaccum) root_coordinate_system scobj) : sceneaccum_objs sceneaccum }

instance (ScenicAccumulator sa) => ScenicAccumulator (a,sa) where
    accumulateScene slayer scobj (a,sceneaccum) = (a,accumulateScene slayer scobj sceneaccum)

null_scene_accumulator :: SceneAccumulator
null_scene_accumulator = SceneAccumulator [] root_coordinate_system

sceneObject :: IO IntermediateModel -> SceneObject
sceneObject = cameraRelativeSceneObject . const . liftM wrapAffine

cameraRelativeSceneObject :: (Camera -> IO (WrappedAffine IntermediateModel)) -> SceneObject
cameraRelativeSceneObject = Model

lightSource :: LightSource -> SceneObject
lightSource = LightSource

accumulateSceneM :: (ScenicAccumulator sa,Monad m,MonadState sa m) => SceneLayer -> SceneObject -> m ()
accumulateSceneM slayer scobj = modify (accumulateScene slayer scobj)

accumulateSceneA :: (ScenicAccumulator sa,Arrow arr,ArrowState sa arr) => arr (SceneLayer,SceneObject) ()
accumulateSceneA = proc (slayer,scobj) ->
    do sceneaccum <- fetch -< ()
       store -< accumulateScene slayer scobj sceneaccum
\end{code}

\subsection{Scene Assembly}

Once all objects have been accumulated, the accumulation is used to generate a \texttt{Scene} object.

\begin{code}
data SceneElement = SceneElement {
    scene_elem_layer :: SceneLayer,
    scene_elem_opaque :: Bool,
    scene_elem_model :: WrappedAffine IntermediateModel,
    scene_elem_light_sources :: [LightSource] }

data Scene = Scene { 
    scene_elements :: Map.Map (SceneLayer,Bool) [SceneElement],
    scene_layerToCamera :: (SceneLayer -> Camera) }

assembleScene :: (SceneLayer -> Camera) -> 
                 (SceneLayer -> LightSource -> SceneLayer -> LightSource)-> 
                 SceneAccumulator -> 
		 IO Scene
assembleScene layerToCamera lightSourceLayerTransform scene_accum = 
    do elements <- liftM (Map.mapWithKey (\(_,opaque) -> if not opaque then sortModels else id) .
		          foldr (\se -> Map.alter (Just . (se:) . fromMaybe []) 
			         (scene_elem_layer se,scene_elem_opaque se)) Map.empty . concat) $
                          mapM toElement $ sceneaccum_objs scene_accum
       return $ Scene { scene_elements = elements,
                        scene_layerToCamera = layerToCamera }
    where splitOpaquesWrapped :: WrappedAffine IntermediateModel -> (WrappedAffine IntermediateModel,
                                                                     [WrappedAffine IntermediateModel])
          splitOpaquesWrapped (WrappedAffine a m) =
                  let (opaques,transparents) = splitOpaques m
                      in (WrappedAffine a opaques,map (WrappedAffine a) transparents) 
          toLightSource :: SceneLayer -> (SceneLayer,SceneObject) -> LightSource
	  toLightSource entering_layer (originating_layer,LightSource ls) = 
	      lightSourceLayerTransform entering_layer ls originating_layer
	  toLightSource _ _ = NoLight
	  sortModels :: [SceneElement] -> [SceneElement]
	  sortModels = map fst . sortBy (comparing $ \(se,bbox) -> negate $ 
	                   minimalDistanceToBoundingBox (camera_position $ layerToCamera $ scene_elem_layer se) bbox) .
                       map (\(se@(SceneElement { scene_elem_model = WrappedAffine cs m })) -> 
		             (se,migrateToFrom cs root_coordinate_system $ boundingBox m))
	  toElement :: (SceneLayer,SceneObject) -> IO [SceneElement]
          toElement (n,Model f) = 
	      do (opaque,transparents) <- liftM splitOpaquesWrapped $ f (layerToCamera n)
	         let light_sources = filter (not . isNoLight) $ map (toLightSource n) (sceneaccum_objs scene_accum) 
		 let base_element = SceneElement {
		     scene_elem_layer = n,
		     scene_elem_opaque = True,
		     scene_elem_model = opaque,
		     scene_elem_light_sources = light_sources }
		 return $ base_element : map (\m -> base_element { scene_elem_model = m,
		                                                   scene_elem_opaque = False }) transparents
          toElement _ = return []

sceneToOpenGL :: Double -> (Double,Double) -> Scene -> IO ()
sceneToOpenGL aspect_ratio nearfar s =
    do let ns = reverse $ Set.toList $ Set.map fst $ Map.keysSet $ scene_elements s
       mapM_ (render1Layer aspect_ratio nearfar s) ns

render1Layer :: Double -> (Double,Double) -> Scene -> SceneLayer -> IO ()
render1Layer aspect_ratio nearfar (Scene elems layerToCamera) n =
    do save_rescale_normal <- GLUT.get rescaleNormal
       save_cull_face <- GLUT.get cullFace
       save_depth_func <- GLUT.get depthFunc
       save_depth_mask <- GLUT.get depthMask
       save_lighting <- GLUT.get lighting
       save_light_model_ambient <- GLUT.get lightModelAmbient
       rescaleNormal $= Enabled
       cullFace $= Just Front
       depthFunc $= Just Lequal
       depthMask $= Enabled
       lighting $= Enabled
       lightModelAmbient $= (Color4 0 0 0 1)
       clear [DepthBuffer]
       preservingMatrix $ 
           do cameraToOpenGL aspect_ratio nearfar (layerToCamera n)
              mapM_ render1Element $ fromMaybe [] $ Map.lookup (n,True) elems
              depthMask $= Disabled
              mapM_ render1Element $ fromMaybe [] $ Map.lookup (n,False) elems
       lightModelAmbient $= save_light_model_ambient
       lighting $= save_lighting
       depthMask $= save_depth_mask
       depthFunc $= save_depth_func
       cullFace $= save_cull_face
       rescaleNormal $= save_rescale_normal

render1Element :: SceneElement -> IO ()
render1Element (SceneElement { scene_elem_light_sources = lss, scene_elem_model = (WrappedAffine m imodel)}) =
    do setLightSources lss
       transformation (migrateToFrom m root_coordinate_system) $ intermediateModelToOpenGL imodel
\end{code}

\subsection{Standard Scene Layers}

This is an example of how to implement scene layers that should be adequate to most purposes.

\begin{code}
stdSceneLayers :: Camera -> SceneLayer -> Camera
stdSceneLayers c sl | sl <= std_scene_layer_hud = c
stdSceneLayers c sl | sl == std_scene_layer_cockpit = c {
    camera_position = origin_point_3d,
    camera_lookat = Point3D 0 0 (-1),
    camera_up = Vector3D 0 1 0 }
stdSceneLayers c sl | sl == std_scene_layer_local = c
stdSceneLayers c sl | sl >= std_scene_layer_infinite = infiniteCameraOf c
stdSceneLayers _ _ = error "stdSceneLayers: impossible case"

std_scene_layer_hud :: SceneLayer
std_scene_layer_hud = 0
std_scene_layer_cockpit :: SceneLayer
std_scene_layer_cockpit = 1
std_scene_layer_local :: SceneLayer
std_scene_layer_local = 2
std_scene_layer_infinite :: SceneLayer
std_scene_layer_infinite = 3
\end{code}

\subsection{Standard Light Layer Transforms}

\begin{code}
defaultLightSourceLayerTransform :: (SceneLayer -> Camera) -> SceneLayer -> LightSource -> SceneLayer -> LightSource
defaultLightSourceLayerTransform _ entering_layer ls originating_layer | entering_layer == originating_layer = ls
defaultLightSourceLayerTransform layerToCamera entering_layer ls originating_layer | entering_layer < originating_layer =
    cameraOrientation (layerToCamera entering_layer) $ infiniteLightSourceOf $ cameraLookAt (layerToCamera originating_layer) ls
defaultLightSourceLayerTransform _ _ _ _ = NoLight
\end{code}
