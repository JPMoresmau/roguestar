module Models.PhaseWeapons
    (phase_pistol)
    where

import Quality
import Model
import Math3D
import Models.ConcordanceStarshipParts



phase_weapon_emitter :: Quality -> Model
phase_weapon_emitter q = rotate (Vector3D 1 0 0) (pi/2) $
                      qualityDeformedSor q dfn concordance_material_metal $
                          points2d [(0,0),
                                    (4,1),
                                    (7,2),
                                    (9,3),
                                    (10,0),
                                    (9,-3),
                                    (7,-6),
                                    (4,-9),
                                    (2,-10),
                                    (0,-10)]
                          where dfn_squish p@(Point3D _ _ z) | z > 0 = scale (Vector3D 1 1 0.1) p
                                dfn_squish p = scale (Vector3D 1 1 0.25) p
                                dfn_smoothe_dish p@(Point3D _ y _) | y > 0 = scale (Vector3D 1 (u/10) 1) p
                                    where u = distanceBetween origin_point_3d $ scale (Vector3D 1 0 1) p
                                dfn_smoothe_dish p = p
                                dfn = dfn_smoothe_dish . dfn_squish
                                 
phase_weapon_grip :: Quality -> Model
phase_weapon_grip q = qualityDeformedSor q dfn concordance_material_metal $
                          points2d [(3,0),
                                    (2,-4),
                                    (1,-7),
                                    (1,-10),
                                    (0,-10)]
                          where dfn_flat_back p@(Point3D _ _ z) | z < 0 = scale (Vector3D (1/r) 1 (1/r)) p 
                                    where r = distanceBetween origin_point_3d $ scale (Vector3D 1 0 1) p
                                dfn_flat_back p = p
                                dfn_gripped_front p@(Point3D _ y z) | z > 0 = scale (Vector3D u 1 u) p 
                                    where u = 1 + (y * sin (pi*y) / 40)
                                dfn_gripped_front p = p
                                dfn_flat_sides (Point3D x y z) = Point3D ((signum x *) $ sqrt (abs x/3)) y z
                                dfn_slope_backwards pt@(Point3D _ y _) = translate (Vector3D 0 0 (-y/3)) pt
                                dfn = dfn_slope_backwards . dfn_flat_back . dfn_flat_sides . dfn_gripped_front
                                
phase_pistol :: Quality -> Model
phase_pistol q = scaleModel 0.2 $
                 Union [translate (Vector3D 0 5 7) $ phase_weapon_emitter q,
                 translate (Vector3D 0 5 0) $ phase_weapon_grip q]