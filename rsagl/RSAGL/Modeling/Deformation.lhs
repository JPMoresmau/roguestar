\section{Non-affine Transformations}

The \texttt{Deformation} typeclass describes any affine or non-affine transformation.

\texttt{Deformation}s fall into two catagories: those that compute their own surface normals and those that don't.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Modeling.Deformation
    (Deformation,DeformationClass(..),constrain)
    where

import RSAGL.Math.Vector
import RSAGL.Math.Matrix
import RSAGL.Math.Affine
import RSAGL.Auxiliary.Auxiliary
import RSAGL.Scene.CoordinateSystems

type Deformation = Either (SurfaceVertex3D -> Point3D) (SurfaceVertex3D -> SurfaceVertex3D)

class DeformationClass a where
    deformation :: a -> Deformation

instance (DeformationClass a,DeformationClass b) => DeformationClass (Either a b) where
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
    deformation f = Left (\(SurfaceVertex3D p _) -> affine_transformation (f p) p)

instance DeformationClass (SurfaceVertex3D -> Affine) where
    deformation f = Left (\s -> affine_transformation (f s) $ sv3d_position s)    
\end{code}

