\section{Scenes and Animation}

A \texttt{Scene} is a complete description of an image to be rendered, consisting of a camera position, light sources, and models.

\begin{code}

{-# OPTIONS_GHC -farrows #-}

module RSAGL.Scene
    (Scene,
     Camera(..),
     LightSource(..),
     SceneObject,
     ScenicAccumulator(..),
     SceneAccumulator,
     sceneObject,
     lightSource,
     accumulateSceneM,
     accumulateSceneA)
    where

import RSAGL.Vector
import RSAGL.Affine
import RSAGL.Model
import RSAGL.CSN
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Control.Arrow.Operations
import RSAGL.Color
import Graphics.UI.GLUT as GLUT
import Data.Maybe
\end{code}

\subsection{Cameras}

\begin{code}
data Camera =
    PerspectiveCamera { camera_position, camera_lookat :: Point3D, 
                        camera_up :: Vector3D }

cameraToOpenGL :: Camera -> IO ()
cameraToOpenGL (PerspectiveCamera { camera_position = (Point3D px py pz),
                                    camera_lookat = (Point3D lx ly lz),
                                    camera_up = (Vector3D ux uy uz)}) =
    lookAt (Vertex3 px py pz) (Vertex3 lx ly lz) (Vector3 ux uy uz)
\end{code}

\subsection{Light Sources}

\begin{code}
data LightSource =
      DirectionalLight { lightsource_direction :: Vector3D,
                         lightsource_color :: RGB }
    | PointLight { lightsource_position :: Point3D,
                   lightsource_radius :: Distance,
                   lightsource_color :: RGB }

makeInfinite :: LightSource -> LightSource
makeInfinite (d@(DirectionalLight {})) = d
makeInfinite (p@PointLight {}) = DirectionalLight {
    lightsource_direction = vectorToFrom origin_point_3d (lightsource_position p),
    lightsource_color = scaleRGB (realToFrac $ 
        (distanceSquared $ lightsource_radius p) / (distanceBetweenSquared origin_point_3d (lightsource_position p))^2) $ lightsource_color p }

instance AffineTransformable LightSource where
    transform m (dl@(DirectionalLight {})) = dl { lightsource_direction = transform m $ lightsource_direction dl }
    transform m (pl@(PointLight {})) = pl {
        lightsource_position = transform m $ lightsource_position pl,
        lightsource_radius = transform m $ lightsource_radius pl }
\end{code}

\subsection{Scene Construction}

A \texttt{Scene} supports local and infinite scene layers.  The camera moves through the local scene layer, but the infinite scene layer is fixed.  Objects in the infinite scene layer never occlude objects in the local layer.  All light sources in the infinite scene layer are rendered as directional light sources in the local scene layer.  Local light sources are not rendered at all in the infinite layer.

Celestial objects such as the moon and sun, as well as the sky sphere, belong in the infinite subscene.  Distant clouds or mountains may also belong in the infinite layer.

\begin{code}
data SceneObject = 
    LightSource LightSource
  | Model (WrappedAffine IntermediateModel)

instance AffineTransformable SceneObject where
    transform m (LightSource ls) = LightSource $ transform m ls
    transform m (Model imodel) = Model $ transform m imodel

data SceneLayer = Local | Infinite deriving (Eq)

data SceneAccumulator = SceneAccumulator {
    sceneaccum_objs :: [(SceneLayer,SceneObject)],
    sceneaccum_coordinate_system :: CoordinateSystem }

instance CoordinateSystemClass SceneAccumulator where
    getCoordinateSystem = sceneaccum_coordinate_system
    storeCoordinateSystem cs accum = accum { sceneaccum_coordinate_system = cs }

class (CoordinateSystemClass a) => ScenicAccumulator a where
    accumulateScene :: SceneLayer -> SceneObject -> a -> a

instance ScenicAccumulator SceneAccumulator where
    accumulateScene slayer scobj accum = accum { 
        sceneaccum_objs = (slayer,migrate (sceneaccum_coordinate_system accum) root_coordinate_system scobj) : sceneaccum_objs accum }

sceneObject :: IntermediateModel -> SceneObject
sceneObject = Model . wrapAffine

lightSource :: LightSource -> SceneObject
lightSource = LightSource

accumulateSceneM :: (ScenicAccumulator sa,Monad m,MonadState sa m) => SceneLayer -> SceneObject -> m ()
accumulateSceneM slayer scobj = modify (accumulateScene slayer scobj)

accumulateSceneA :: (ScenicAccumulator sa,Arrow arr,ArrowState sa arr) => arr (SceneLayer,SceneObject) ()
accumulateSceneA = proc (slayer,scobj) ->
    do accum <- fetch -< ()
       store -< accumulateScene slayer scobj accum
\end{code}

\subsection{Scene Assembly}

Once all objects have been accumulated, the accumulation is used to generate a \texttt{Scene} object.  

\begin{code}
data Scene = Scene {
    scene_infinite_objs :: [(WrappedAffine IntermediateModel,[LightSource])],
    scene_local_objs :: [(WrappedAffine IntermediateModel,[LightSource])],
    scene_camera :: Camera }

assembleScene :: Camera -> SceneAccumulator -> Scene
assembleScene c sceneaccum = Scene {
    scene_infinite_objs = map (\m -> (m,infinite_light_sources)) local_models,
    scene_local_objs = map (\m -> (m,local_light_sources)) local_models,
    scene_camera = c }
        where infinites = map snd $ filter ((Infinite ==) . fst) $ sceneaccum_objs sceneaccum
              locals = map snd $ filter ((Local ==) . fst) $ sceneaccum_objs sceneaccum
              infinite_light_sources = mapMaybe toLightSource infinites
              local_light_sources = map makeInfinite infinite_light_sources ++ mapMaybe toLightSource locals
              infinite_models = mapMaybe toModel infinites
              local_models = mapMaybe toModel locals
              toLightSource so = case so of
                  LightSource ls -> Just ls
                  _ -> Nothing
              toModel so = case so of
                  Model m -> Just m
                  _ -> Nothing
\end{code}