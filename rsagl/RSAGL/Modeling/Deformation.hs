{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Modeling.Deformation
    (Deformation,DeformationClass(..),constrain)
    where

import RSAGL.Math.Vector
import RSAGL.Math.Matrix
import RSAGL.Math.Affine
import RSAGL.Auxiliary.Auxiliary
import RSAGL.Scene.CoordinateSystems

-- | A deformation of a surface.  These come in two types: a deformation that
-- modifies only the shape of a surface, leaving the normal vectors to
-- be recalculated automatically, or a deformation that modified both
-- the shape and the normal vectors explicitly.
--
-- For example, the deformation function of type @(Point3D -> Ponit3D)@
-- depends on automatic differentiation to determine the new surface normals.
--
-- On the other hand, an affine transformation applied as a deformation
-- can quickly and correctly compute the new surface normals.
--
-- Finally, one might want to construct a deformation that modifies
-- the surface normals while leaving the shape intact -- this is
-- perfectly legal.
--
type Deformation = Either (SurfaceVertex3D -> Point3D) (SurfaceVertex3D -> SurfaceVertex3D)

-- | A convenienve class to convert common descriptions of deformations
-- into the canonical representation.
class DeformationClass a where
    deformation :: a -> Deformation

instance (DeformationClass a,DeformationClass b) =>
         DeformationClass (Either a b) where
    deformation = either deformation deformation

instance DeformationClass Matrix where
    deformation m = Right (\s -> transform m s)

instance DeformationClass (Point3D -> Point3D) where
    deformation f = Left (\(SurfaceVertex3D p _) -> f p)

instance DeformationClass (Point3D -> SurfaceVertex3D) where
    deformation f = Right (\(SurfaceVertex3D p _) -> f p)

instance DeformationClass (SurfaceVertex3D -> Point3D) where
    deformation f = Left f

instance DeformationClass (SurfaceVertex3D -> SurfaceVertex3D) where
    deformation f = Right f

instance DeformationClass (Point3D -> Affine) where
    deformation f = Left (\(SurfaceVertex3D p _) ->
                        affine_transformation (f p) p)

instance DeformationClass (SurfaceVertex3D -> Affine) where
    deformation f = Left (\s -> affine_transformation (f s) $ sv3d_position s)

