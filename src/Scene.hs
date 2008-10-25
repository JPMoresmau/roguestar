module Scene
    (module RSAGL.Scene,
     roguestarSceneLayerInfo,
     scene_layer_hud,
     scene_layer_cockpit,
     scene_layer_local,
     scene_layer_near_sky,
     scene_layer_clouds,
     scene_layer_sky_sphere,
     scene_layer_far_sky,
     scene_layer_orbit,
     scene_layer_distant)
    where

import RSAGL.Scene hiding (std_scene_layer_hud,std_scene_layer_cockpit,std_scene_layer_local,std_scene_layer_infinite)
import qualified RSAGL.Scene as SceneLayers (std_scene_layer_hud,std_scene_layer_cockpit,std_scene_layer_local,std_scene_layer_infinite)
import Data.Monoid

roguestarSceneLayerInfo :: LightSourceLayerTransform -> Camera -> SceneLayerInfo
roguestarSceneLayerInfo light_source_layer_transform c = SceneLayerInfo
    (stdSceneLayers c)
    (light_source_layer_transform `mappend` cameraLightSourceLayerTransform (stdSceneLayers c))

scene_layer_hud :: SceneLayer
scene_layer_hud = SceneLayers.std_scene_layer_hud

scene_layer_cockpit :: SceneLayer
scene_layer_cockpit = SceneLayers.std_scene_layer_cockpit

scene_layer_local :: SceneLayer
scene_layer_local = SceneLayers.std_scene_layer_local

-- | Infinite layer, under the clouds
scene_layer_near_sky :: SceneLayer
scene_layer_near_sky = SceneLayers.std_scene_layer_infinite

-- | Contains clouds only
scene_layer_clouds :: SceneLayer
scene_layer_clouds = SceneLayers.std_scene_layer_infinite + 1

-- | Infinite layer, above clouds
scene_layer_far_sky :: SceneLayer
scene_layer_far_sky = SceneLayers.std_scene_layer_infinite + 2

-- | Contains sky sphere only
scene_layer_sky_sphere :: SceneLayer
scene_layer_sky_sphere = SceneLayers.std_scene_layer_infinite + 3

-- | Infinite layer, above the sky
scene_layer_orbit :: SceneLayer
scene_layer_orbit = SceneLayers.std_scene_layer_infinite + 4

-- | Infinite layer, contains astronomical objects
scene_layer_distant :: SceneLayer
scene_layer_distant = SceneLayers.std_scene_layer_infinite + 5
