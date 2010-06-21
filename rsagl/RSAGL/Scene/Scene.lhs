\section{Scenes and Animation}

A \texttt{Scene} is a complete description of an image to be rendered, consisting of a camera position, light sources, and models.

\begin{code}

{-# LANGUAGE Arrows, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}

module RSAGL.Scene.Scene
    (Scene,
     Camera(..),
     infiniteCameraOf,
     SceneLayerInfo(..),
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
     stdSceneLayerInfo,
     stdSceneLayers,
     std_scene_layer_hud,
     std_scene_layer_cockpit,
     std_scene_layer_local,
     std_scene_layer_infinite,
     LightSourceLayerTransform(..),
     cameraLightSourceLayerTransform)
    where

import Data.Ord
import RSAGL.Modeling.BoundingBox
import RSAGL.Math.Vector
import RSAGL.Math.Affine as Affine
import RSAGL.Math.Angle as Angle
import RSAGL.Modeling.Model
import RSAGL.Scene.CoordinateSystems
import Data.List
import Control.Monad.State as State
import Control.Arrow
import Control.Arrow.Operations
import Graphics.Rendering.OpenGL as GL
import Data.Maybe
import RSAGL.Math.WrappedAffine
import RSAGL.Math.Orthogonal
import RSAGL.Scene.LightSource
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid
import RSAGL.Auxiliary.RecombinantState
import Data.MemoCombinators
import RSAGL.Types
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

cameraToOpenGL :: RSdouble -> (RSdouble,RSdouble) -> Camera -> IO ()
cameraToOpenGL aspect_ratio (near,far)
        (PerspectiveCamera { camera_position = (Point3D px py pz),
                             camera_lookat = (Point3D lx ly lz),
                             camera_up = (Vector3D ux uy uz),
                             camera_fov = fov }) =
    do matrixMode $= Projection
       loadIdentity
       perspective (f2f $ toDegrees fov)
                   (f2f aspect_ratio)
                   (f2f near)
                   (f2f far)
       matrixMode $= Modelview 0
       lookAt (Vertex3 (f2f px) (f2f py) (f2f pz)) 
              (Vertex3 (f2f lx) (f2f ly) (f2f lz)) 
              (Vector3 (f2f ux) (f2f uy) (f2f uz))

infiniteCameraOf :: Camera -> Camera
infiniteCameraOf pc = translateToFrom origin_point_3d (camera_position pc) pc

cameraOrientation :: (AffineTransformable a) => Camera -> a -> a
cameraOrientation c = modelLookAt (camera_position c) (forward $ Left $ camera_lookat c) 
                                                      (up $ Right $ camera_up c)

cameraLookAt :: (AffineTransformable a) => Camera -> a -> a
cameraLookAt = inverseTransformation . cameraOrientation 
\end{code}

\subsection{Scene Construction}

A \texttt{Scene} supports local and infinite scene layers.  The camera moves through the local scene layer, but the infinite scene layer is fixed.  Objects in the infinite scene layer never occlude objects in the local layer.  All light sources in the infinite scene layer are rendered as directional light sources in the local scene layer.  Local light sources are not rendered at all in the infinite layer.

Celestial objects such as the moon and sun, as well as the sky sphere, belong in the infinite subscene.  Distant clouds or mountains may also belong in the infinite layer.

\begin{code}
data SceneObject m = 
    LightSource LightSource
  | Model (Camera -> m (WrappedAffine IntermediateModel))

instance (Monad m) => AffineTransformable (SceneObject m) where
    transform m (LightSource ls) = LightSource $ transform m ls
    transform m (Model imodel) = Model $ \c -> liftM (transform m) (imodel c)

type SceneLayer = Integer

data SceneAccumulator m = SceneAccumulator {
    sceneaccum_objs :: [(SceneLayer,SceneObject m)],
    sceneaccum_coordinate_system :: CoordinateSystem }

instance CoordinateSystemClass (SceneAccumulator m) where
    getCoordinateSystem = sceneaccum_coordinate_system
    storeCoordinateSystem cs sceneaccum = sceneaccum { sceneaccum_coordinate_system = cs }

instance RecombinantState (SceneAccumulator m) where
    type SubState (SceneAccumulator m) = SceneAccumulator m
    clone orig = orig { sceneaccum_objs = [] }
    recombine orig new = orig {
        sceneaccum_objs = sceneaccum_objs new ++ sceneaccum_objs orig }

class (RecombinantState a,CoordinateSystemClass a,Monad m) => ScenicAccumulator a m | a -> m where -- REVISIT: fundeps just for this, really?
    accumulateScene :: SceneLayer -> SceneObject m -> a -> a

instance (Monad m) => ScenicAccumulator (SceneAccumulator m) m where
    accumulateScene slayer scobj sceneaccum = sceneaccum { 
        sceneaccum_objs = (slayer,migrateToFrom (sceneaccum_coordinate_system sceneaccum) root_coordinate_system scobj) : sceneaccum_objs sceneaccum }

null_scene_accumulator :: SceneAccumulator m
null_scene_accumulator = SceneAccumulator [] root_coordinate_system

sceneObject :: (Monad m,ModelType mt) => m mt -> SceneObject m
sceneObject = cameraRelativeSceneObject . const . liftM (wrapAffine . toIntermediateModel)

cameraRelativeSceneObject :: (Monad m) => (Camera -> m (WrappedAffine IntermediateModel)) -> SceneObject m
cameraRelativeSceneObject = Model

lightSource :: LightSource -> SceneObject m
lightSource = LightSource

accumulateSceneM :: (ScenicAccumulator sa a,Monad m,MonadState sa m) => SceneLayer -> SceneObject a -> m ()
accumulateSceneM slayer scobj = modify (accumulateScene slayer scobj)

accumulateSceneA :: (ScenicAccumulator sa m,Arrow arr,ArrowState sa arr) => arr (SceneLayer,SceneObject m) ()
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

data SceneLayerInfo = SceneLayerInfo {
    scene_layer_camera :: SceneLayer -> Camera,
    scene_layer_light_source_layer_transform :: LightSourceLayerTransform }

assembleScene :: (Monad m) => SceneLayerInfo -> SceneAccumulator m -> m Scene
assembleScene (SceneLayerInfo layerToCamera light_source_layer_transform) scene_accum = 
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
          toLightSource :: SceneLayer -> (SceneLayer,SceneObject m) -> LightSource
	  toLightSource entering_layer (originating_layer,LightSource ls) = 
	      lightSourceLayerTransform light_source_layer_transform entering_layer originating_layer ls
	  toLightSource _ _ = NoLight
	  sortModels :: [SceneElement] -> [SceneElement]
	  sortModels = map fst . sortBy (comparing $ \(se,bbox) -> negate $ 
	                   minimalDistanceToBoundingBox (camera_position $ layerToCamera $ scene_elem_layer se) bbox) .
                       map (\(se@(SceneElement { scene_elem_model = WrappedAffine cs m })) -> 
		             (se,migrateToFrom cs root_coordinate_system $ boundingBox m))
	  toElement :: (Monad m) => (SceneLayer,SceneObject m) -> m [SceneElement]
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

sceneToOpenGL :: RSdouble -> (RSdouble,RSdouble) -> Scene -> IO ()
sceneToOpenGL aspect_ratio nearfar s =
    do let ns = reverse $ Set.toList $ Set.map fst $ Map.keysSet $ scene_elements s
       mapM_ (render1Layer aspect_ratio nearfar s) ns

render1Layer :: RSdouble -> (RSdouble,RSdouble) -> Scene -> SceneLayer -> IO ()
render1Layer aspect_ratio nearfar (Scene elems layerToCamera) n =
    do save_rescale_normal <- GL.get rescaleNormal
       save_cull_face <- GL.get cullFace
       save_depth_func <- GL.get depthFunc
       save_depth_mask <- GL.get depthMask
       save_lighting <- GL.get lighting
       save_light_model_ambient <- GL.get lightModelAmbient
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
    do setLightSourcesToOpenGL lss
       migrateToFrom m root_coordinate_system $ intermediateModelToOpenGL imodel
\end{code}

\subsection{Standard Scene Layers}

This is an example of how to implement scene layers that should be adequate to most purposes.

\begin{code}
stdSceneLayerInfo :: Camera -> SceneLayerInfo
stdSceneLayerInfo c = SceneLayerInfo (stdSceneLayers c) (cameraLightSourceLayerTransform (stdSceneLayers c))

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
newtype LightSourceLayerTransform = LightSourceLayerTransform { lightSourceLayerTransform :: SceneLayer -> SceneLayer -> LightSource -> LightSource }

instance Monoid LightSourceLayerTransform where
    mempty = LightSourceLayerTransform $ const $ const id
    mappend (LightSourceLayerTransform f) (LightSourceLayerTransform g) = LightSourceLayerTransform $ memo2 integral integral $ \a b -> f a b . g a b

-- | Performs the minimal light source layer transform needed to maintain correct light sources under camera transformations.
cameraLightSourceLayerTransform :: (SceneLayer -> Camera) -> LightSourceLayerTransform
cameraLightSourceLayerTransform layerToCamera = LightSourceLayerTransform $ memo2 integral integral f
    where f :: SceneLayer -> SceneLayer -> LightSource -> LightSource
          f  entering_layer originating_layer | entering_layer == originating_layer = id
          f entering_layer originating_layer | entering_layer < originating_layer =
              cameraOrientation (layerToCamera entering_layer) . infiniteLightSourceOf . cameraLookAt (layerToCamera originating_layer)
          f _ _ = const NoLight
\end{code}
