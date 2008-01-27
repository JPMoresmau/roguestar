\section{Scenes and Animation}

A \texttt{Scene} is a complete description of an image to be rendered, consisting of a camera position, light sources, and models.

\begin{code}

{-# OPTIONS_GHC -farrows #-}

module RSAGL.Scene
    (Scene,
     Camera(..),
     LightSource(..),
     SceneObject,
     SceneLayer(..),
     ScenicAccumulator(..),
     SceneAccumulator,
     null_scene_accumulator,
     sceneObject,
     cameraRelativeSceneObject,
     lightSource,
     accumulateSceneM,
     accumulateSceneA,
     assembleScene,
     sceneToOpenGL)
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

infiniteCameraToOpenGL :: Double -> (Double,Double) -> Camera -> IO ()
infiniteCameraToOpenGL aspect_ratio nearfar pc =
    cameraToOpenGL aspect_ratio nearfar $ Affine.translate (vectorToFrom origin_point_3d $ camera_position pc) pc
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

makeInfinite :: LightSource -> LightSource
makeInfinite NoLight = NoLight
makeInfinite (d@(DirectionalLight {})) = d
makeInfinite (p@PointLight {}) = DirectionalLight {
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

data SceneLayer = Local | Infinite deriving (Eq)

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
        sceneaccum_objs = (slayer,migrate (sceneaccum_coordinate_system sceneaccum) root_coordinate_system scobj) : sceneaccum_objs sceneaccum }

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
data Scene = Scene {
    scene_infinite_opaques :: [(WrappedAffine IntermediateModel,[LightSource])],
    scene_infinite_transparents :: [(WrappedAffine IntermediateModel,[LightSource])],
    scene_local_opaques :: [(WrappedAffine IntermediateModel,[LightSource])],
    scene_local_transparents :: [(WrappedAffine IntermediateModel,[LightSource])],
    scene_camera :: Camera }

-- FIXME: This function is a horrible mess (I need to redo this to implement 0.0.4 features anyway).
assembleScene :: Camera -> SceneAccumulator -> IO Scene
assembleScene c sceneaccum = 
    do infinite_models <- liftM (map splitOpaquesWrapped . catMaybes) $ mapM toModel infinites
       local_models <- liftM (map splitOpaquesWrapped . catMaybes) $ mapM toModel locals
       return $ Scene {
           scene_infinite_opaques = map (\m -> (fst m,infinite_light_sources)) infinite_models,
           scene_infinite_transparents = map (\m -> (m,infinite_light_sources)) $ sortModels origin_point_3d $ concatMap snd infinite_models,
           scene_local_opaques = map (\m -> (fst m,local_light_sources)) local_models,
           scene_local_transparents = map (\m -> (m,local_light_sources)) $ sortModels (camera_position c) $ concatMap snd local_models,
           scene_camera = c }
        where infinites = map snd $ filter ((Infinite ==) . fst) $ sceneaccum_objs sceneaccum
              locals = map snd $ filter ((Local ==) . fst) $ sceneaccum_objs sceneaccum
              infinite_light_sources = mapMaybe toLightSource infinites
              local_light_sources = map makeInfinite infinite_light_sources ++ mapMaybe toLightSource locals
              sortModels :: Point3D -> [WrappedAffine IntermediateModel] -> [WrappedAffine IntermediateModel]
              sortModels p = map fst . sortBy (comparing $ negate . minimalDistanceToBoundingBox p . snd) .
                             map (\(wa@(WrappedAffine cs m)) -> (wa,migrate cs root_coordinate_system $ boundingBox m))
              splitOpaquesWrapped (WrappedAffine a m) =
                  let (opaques,transparents) = splitOpaques m
                      in (WrappedAffine a opaques,map (WrappedAffine a) transparents)
              toLightSource so = case so of
                  LightSource ls -> Just ls
                  _ -> Nothing
              toModel so = case so of
                  Model m -> liftM Just (m c)
                  _ -> return Nothing

sceneToOpenGL :: Double -> (Double,Double) -> Scene -> IO ()
sceneToOpenGL aspect_ratio nearfar scene =
    do rescaleNormal $= Enabled
       cullFace $= Just Front
       depthFunc $= Just Lequal
       depthMask $= Enabled
       lighting $= Enabled
       lightModelAmbient $= (Color4 0 0 0 1)
       clear [DepthBuffer]
       preservingMatrix $ 
           do infiniteCameraToOpenGL aspect_ratio nearfar (scene_camera scene)
              mapM_ render1Object (scene_infinite_opaques scene)
              mapM_ render1Object (scene_infinite_transparents scene)
       clear [DepthBuffer]
       preservingMatrix $ 
           do cameraToOpenGL aspect_ratio nearfar (scene_camera scene)
              mapM_ render1Object (scene_local_opaques scene)
              mapM_ render1Object (scene_local_transparents scene)

render1Object :: (WrappedAffine IntermediateModel,[LightSource]) -> IO ()
render1Object (WrappedAffine m imodel,lss) =
    do setLightSources lss
       transformation (migrate m root_coordinate_system) $ intermediateModelToOpenGL imodel
\end{code}