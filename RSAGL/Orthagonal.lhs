\section{Orthagonal Unit-Scaled Coordinate Systems}

It's useful to work with the set of coordinate systems restricted to those that use orthagonal unit-scaled axes, that is, that are subject only to rotation and translation.
This is because these coordinate systems are the only ones that describe rigid objects.

\begin{code}
module RSAGL.Orthagonal
    (up,down,left,right,forward,backward,
     orthagonalFrame,
     modelLookAt,
     FUR)
    where

import RSAGL.Affine
import RSAGL.Vector
import RSAGL.Matrix
\end{code}

\texttt{FUR} stands for Forward Up Right.  It's used to specify arbitrary orthagonal coordinate systems given any combination
of forward up and right vectors.  It also accepts down, left, and backward vectors.

\texttt{right} is positive X, \texttt{up} is positive Y, \texttt{forward} is positive Z.

When specifying \texttt{FUR} coordinate systems, the first vector is fixed, while the second vector will be adjusted as little as possible to 
guarantee that it is orthagonal to the first.  The third vector never needs to be specified, it can be deduced.

\begin{code}
data FURAxis = ForwardAxis | UpAxis | RightAxis | DownAxis | LeftAxis | BackwardAxis

data FUR a = FUR FURAxis a

instance Functor FUR where
    fmap f (FUR a x) = FUR a $ f x

up :: a -> FUR a
up = FUR UpAxis

down :: a -> FUR a
down = FUR DownAxis

left :: a -> FUR a
left = FUR LeftAxis

right :: a -> FUR a
right = FUR RightAxis

forward :: a -> FUR a
forward = FUR ForwardAxis

backward :: a -> FUR a
backward = FUR BackwardAxis

orthagonalFrame :: (AffineTransformable a) => FUR Vector3D -> FUR Vector3D -> a -> a
orthagonalFrame (FUR ForwardAxis f) (FUR RightAxis r) = let (r',u') = fixOrtho2 f r in transform (xyzMatrix r' u' (vectorNormalize f))
orthagonalFrame (FUR UpAxis u) (FUR ForwardAxis f) = let (f',r') = fixOrtho2 u f in transform (xyzMatrix r' (vectorNormalize u) f')
orthagonalFrame (FUR RightAxis r) (FUR UpAxis u) = let (u',f') = fixOrtho2 r u in transform (xyzMatrix (vectorNormalize r) u' f')
orthagonalFrame (FUR RightAxis r) (FUR ForwardAxis f) = let (f',u') = fixOrtho2Left r f in transform (xyzMatrix (vectorNormalize r) u' f')
orthagonalFrame (FUR ForwardAxis f) (FUR UpAxis u) = let (u',r') = fixOrtho2Left f u in transform (xyzMatrix r' u' (vectorNormalize f))
orthagonalFrame (FUR UpAxis u) (FUR RightAxis r) = let (r',f') = fixOrtho2Left u r in transform (xyzMatrix r' (vectorNormalize u) f')
orthagonalFrame (FUR ForwardAxis _) (FUR ForwardAxis _) = error "orthagonalFrame: two forward vectors"
orthagonalFrame (FUR UpAxis _) (FUR UpAxis _) = error "orthagonalFrame: two up vectors"
orthagonalFrame (FUR RightAxis _) (FUR RightAxis _) = error "orthagonalFrame: two right vectors"
orthagonalFrame x y = orthagonalFrame (furCorrect x) (furCorrect y)

furCorrect :: FUR Vector3D -> FUR Vector3D
furCorrect (FUR ForwardAxis f) = FUR ForwardAxis f
furCorrect (FUR UpAxis u) = FUR UpAxis u
furCorrect (FUR RightAxis r) = FUR RightAxis r
furCorrect (FUR DownAxis d) = FUR UpAxis $ vectorScale (-1) d
furCorrect (FUR LeftAxis l) = FUR RightAxis $ vectorScale (-1) l
furCorrect (FUR BackwardAxis b) = FUR ForwardAxis $ vectorScale (-1) b
\end{code}

\texttt{modelLookAt} generates the affine transformation needed to aim a model at a given position either at a point or along a vector.
The first parameter is the position of the model.  Typically the second parameter will be the position of the target, and the 
third parameter will \texttt{(up \$ Vector3D 0 1 0)}.

\begin{code}
modelLookAt :: (AffineTransformable a) => Point3D -> FUR (Either Point3D Vector3D) -> FUR (Either Point3D Vector3D) -> a -> a
modelLookAt pos primaryish secondaryish = RSAGL.Affine.translate (vectorToFrom pos origin_point_3d) . orthagonalFrame primary secondary
    where primary = fmap (either (`vectorToFrom` pos) id) primaryish
          secondary = fmap (either (`vectorToFrom` pos) id) secondaryish
\end{code}
