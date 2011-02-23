{-# LANGUAGE Arrows, TypeFamilies #-}

module AnimationVortex
    (Vortex(..),
     vortex,
     particleAvatar)
    where

import RSAGL.FRP
import RSAGL.Animation
import RSAGL.Math
import RSAGL.Scene
import RSAGL.Color
import Animation
import Models.LibraryData
import Scene
import Control.Arrow
import CreatureData
import VisibleObject
import System.Random
import Data.List (genericTake)

-- | A type to represent various vortex-like monsters.
data Vortex = Vortex {
    -- | The height of the center of containment sphere of the vortex.
    vortex_height :: RSdouble,
    -- | The strength of the containment sphere of the vortex.
    vortex_containment :: RSdouble,
    -- | Atmospheric drag.
    vortex_drag :: RSdouble,
    -- | Mutual n-to-n attraction of the particles.  (can be used at the same time as repulsion)
    vortex_binding :: RSdouble,
    -- | Mutual n-to-n repulsion of the particles.
    vortex_repulsion :: RSdouble,
    -- | Enforced rotation of the particles.  This can vary as a function of distance
    -- from the central axis.
    vortex_rotation :: RSdouble -> RSdouble,
    -- | The amount of force applied to a particle that violates
    -- the base angle.
    vortex_base_force :: RSdouble,
    -- | A base.  At 0 degrees, this simulates the ground, but
    -- but at other angles it can enforce a funnel shape.
    vortex_base_angle :: Angle,
    -- | Gravitational force.
    vortex_gravity :: RSdouble }

vortex :: Vortex
vortex = Vortex {
    vortex_height = 0.5,
    vortex_containment = 100,
    vortex_drag = 1.0,
    vortex_binding = 0,
    vortex_repulsion = 1.0,
    vortex_rotation = const 1.0,
    vortex_base_angle = fromDegrees 0,
    vortex_base_force = 10,
    vortex_gravity = 1.0 }

vortexForceFunction :: Vortex -> [(Point3D,Rate Vector3D)] -> ForceFunction
vortexForceFunction v particles =
    concatForces [
        -- Bind the entire system to the origin of the local coordinate system.
        quadraticTrap (vortex_containment v) (Point3D 0 (vortex_height v) 0),
        -- Damp down runaway behavior.
        drag (vortex_drag v),
        -- Repulse points that get too close.
        concatForces $ map (\cloud_point ->
            constrainForce (\_ p _ -> distanceBetween p cloud_point > 0.001)
                           (scalarMultiply (-1) $ inverseSquareLaw (vortex_repulsion v) cloud_point))
            (map fst particles),
        -- Attract points that wonder too far away.
        concatForces $ map (quadraticTrap (vortex_binding v) . fst) particles,
        -- Swirl points around the y axis.
        \_ p _ -> perSecond $ perSecond $
            (vectorNormalize $ vectorToFrom origin_point_3d p) `crossProduct`
            (Vector3D 0 (vortex_rotation v $ distanceBetween origin_point_3d p) 0),
        -- Bounce off the ground.
        constrainForce (\ _ (Point3D x y z) _ ->
                           fromDegrees 90 `sub`
                           angleBetween (vectorToFrom (Point3D x y z) origin_point_3d)
                                        (Vector3D 0 1 0)
                               < vortex_base_angle v) $
            \_ (Point3D x _ z) _ -> perSecond $ perSecond $ vectorScaleTo (vortex_base_force v) $
                vectorScaleTo (sine $ vortex_base_angle v) (Vector3D (-x) 0 (-z)) `add`
                (Vector3D 0 (cosine $ vortex_base_angle v) 0),
        \_ _ _ -> perSecond $ perSecond $ Vector3D 0 (negate $ vortex_gravity v) 0
        ]

glower :: (FRPModel m, FRPModes m ~ RoguestarModes,
           ThreadIDOf m ~ Maybe Integer) =>
          LibraryModel -> FRP e m (Point3D, Rate Vector3D, Acceleration Vector3D) ()
glower library_model = proc (p,_,_) ->
    do local_origin <- exportToA root_coordinate_system -< origin_point_3d
       transformA libraryPointAtCamera -<
           (translateToFrom p origin_point_3d $ -- use absolute positioning
                translateToFrom local_origin origin_point_3d $
                    root_coordinate_system,
            (scene_layer_local,library_model))
       returnA -< ()

random_particles :: [(Point3D,Rate Vector3D)]
random_particles = makeAParticle vs
    where makeAParticle (a:b:c:d:e:f:xs) = (Point3D a (b+0.5) c,perSecond $ Vector3D d e f) : makeAParticle xs
          makeAParticle _ = error "Debauchery is perhaps an act of despair in the face of infinity."
          vs = randomRs (-0.5,0.5) $ mkStdGen 5

particleAvatar :: (FRPModel m) => Vortex -> Integer -> LibraryModel -> (Maybe RGB) -> CreatureAvatar e m
particleAvatar vortex_spec num_particles library_model m_color = genericCreatureAvatar $ proc () ->
    do a <- inertia root_coordinate_system origin_point_3d -< ()
       particles <- particleSystem fps120 (genericTake num_particles random_particles) -<
           \particles -> concatForces [vortexForceFunction vortex_spec particles, \_ _ _ -> a]
       glower library_model -< particles !! 0
       glower library_model -< particles !! 1
       glower library_model -< particles !! 2
       glower library_model -< particles !! 3
       glower library_model -< particles !! 4
       glower library_model -< particles !! 5
       glower library_model -< particles !! 6
       glower library_model -< particles !! 7
       accumulateSceneA -< (scene_layer_local,
                            lightSource $
           case m_color of
               Just color -> PointLight (Point3D 0 0.5 0)
                                        (measure (Point3D 0 0.5 0) (Point3D 0 0 0))
                                        color
                                        color
               Nothing -> NoLight)
       t <- threadTime -< ()
       wield_point <- exportCoordinateSystem -< translate (rotateY (fromRotations $ t `cyclical'` (fromSeconds 3)) $ Vector3D 0.25 0.5 0)
       returnA -< (CreatureThreadOutput {
           cto_wield_point = wield_point })


