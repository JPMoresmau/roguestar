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
\end{code}

