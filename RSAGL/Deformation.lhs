\section{Non-affine Transformations}

The \texttt{Deformation} typeclass describes any affine or non-affine transformation.

\texttt{Deformation}s fall into two catagories: those that compute their own surface normals and those that don't.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Deformation
    (Deformation,DeformationClass(..))
    where

import RSAGL.Vector
import RSAGL.Matrix
import RSAGL.Affine

type Deformation = Either (SurfaceVertex3D -> Point3D) (SurfaceVertex3D -> (Point3D,Vector3D))

class DeformationClass a where
    deformation :: a -> Deformation

instance (DeformationClass a,DeformationClass b) => DeformationClass (Either a b) where
    deformation = either deformation deformation

instance DeformationClass Matrix where
    deformation m = Right (\(SurfaceVertex3D p v _) -> (transform m p,transform m v))

instance DeformationClass (Point3D -> Point3D) where
    deformation f = Left (\(SurfaceVertex3D p _ _) -> f p)

instance DeformationClass (Point3D -> (Point3D,Vector3D)) where
    deformation f = Right (\(SurfaceVertex3D p _ _) -> f p)

instance DeformationClass ((Point3D,Vector3D) -> Point3D) where
    deformation f = Left (\(SurfaceVertex3D p v _) -> f (p,v))

instance DeformationClass ((Point3D,Vector3D) -> (Point3D,Vector3D)) where
    deformation f = Right (\(SurfaceVertex3D p v _) -> f (p,v)) where

instance DeformationClass (SurfaceVertex3D -> (Point3D,Vector3D)) where
    deformation f = Right f

instance DeformationClass (SurfaceVertex3D -> Point3D) where
    deformation f = Left f
\end{code}

