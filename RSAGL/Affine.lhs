\section{Transforming geometric objects: RSAGL.Affine}

AffineTransformable objects are entities that are subject to affine transformations using matrix multiplication.

Defaults are provided for all methods of AffineTransformable except transform.

The IO monad itself is AffineTransformable.  This is done by wrapping the IO action in an OpenGL transformation.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Affine
    (AffineTransformable(..),
     AffineTransformation,
     AffineTransformationType,
     affine_identity,
     transformation,
     inverseTransformation,
     rotateAbout,
     rotateToFrom,
     WrappedAffine(..),wrapAffine,unwrapAffine,
     FUR,forward,up,right,down,left,backward,orthagonalFrame,modelLookAt)
    where

import Graphics.Rendering.OpenGL.GL as GL hiding (R)
import RSAGL.Vector
import RSAGL.Matrix
import RSAGL.Angle
import RSAGL.Homogenous

class AffineTransformable a where
    transform :: RSAGL.Matrix.Matrix -> a -> a
    scale :: Vector3D -> a -> a
    scale vector = transform $ scaleMatrix vector
    translate :: Vector3D -> a -> a
    translate vector = transform $ translationMatrix vector
    rotate :: Vector3D -> Angle -> a -> a
    rotate vector angle = transform $ rotationMatrix vector angle
    rotateX :: Angle -> a -> a
    rotateX = RSAGL.Affine.rotate (Vector3D 1 0 0)
    rotateY :: Angle -> a -> a
    rotateY = RSAGL.Affine.rotate (Vector3D 0 1 0)
    rotateZ :: Angle -> a -> a
    rotateZ = RSAGL.Affine.rotate (Vector3D 0 0 1)
    scale' :: Double -> a -> a
    scale' x = RSAGL.Affine.scale (Vector3D x x x)
    inverseTransform :: RSAGL.Matrix.Matrix -> a -> a
    inverseTransform m = transform (matrixInverse m)

newtype AffineTransformationType = AffineTransformationType RSAGL.Matrix.Matrix deriving (AffineTransformable)
type AffineTransformation = AffineTransformationType -> AffineTransformationType

affine_identity :: AffineTransformationType
affine_identity = AffineTransformationType $ identityMatrix 4

transformation :: (AffineTransformable a) => AffineTransformation -> a -> a
transformation f = transform m
    where AffineTransformationType m = f affine_identity

inverseTransformation :: (AffineTransformable a) => AffineTransformation -> a -> a
inverseTransformation f = inverseTransform m
    where AffineTransformationType m = f affine_identity

rotateAbout :: (AffineTransformable a) => Point3D -> Vector3D -> Angle -> a -> a
rotateAbout center vector angle = 
    RSAGL.Affine.translate (vectorToFrom center origin_point_3d) .
    RSAGL.Affine.rotate vector angle .
    RSAGL.Affine.translate (vectorToFrom origin_point_3d center)

rotateToFrom :: (AffineTransformable a) => Vector3D -> Vector3D -> a -> a
rotateToFrom u v = RSAGL.Affine.rotate c a
    where c = vectorNormalize $ crossProduct u v
          a = angleBetween u v

instance AffineTransformable a => AffineTransformable [a] where
    scale v = map (RSAGL.Affine.scale v)
    translate v = map (RSAGL.Affine.translate v)
    rotate angle vector = map (RSAGL.Affine.rotate angle vector)
    transform m = map (transform m)

instance (AffineTransformable a,AffineTransformable b) => AffineTransformable (a,b) where
    transform m (a,b) = (transform m a,transform m b)

instance (AffineTransformable a,AffineTransformable b,AffineTransformable c) => AffineTransformable (a,b,c) where
    transform m (a,b,c) = (transform m a,transform m b,transform m c)

instance AffineTransformable RSAGL.Matrix.Matrix where
    transform mat = matrixMultiply mat

instance AffineTransformable Vector3D where
    transform = transformHomogenous
    scale (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1*x2) (y1*y2) (z1*z2)
    translate (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1+x2) (y1+y2) (z1+z2)

instance AffineTransformable Point3D where
    transform = transformHomogenous
    scale (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1*x2) (y1*y2) (z1*z2)
    translate (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1+x2) (y1+y2) (z1+z2)

instance AffineTransformable SurfaceVertex3D where
    transform m (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.transform m p) (RSAGL.Affine.transform m v)
    scale vector (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.scale vector p) (RSAGL.Affine.scale vector v)
    translate vector (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.translate vector p) (RSAGL.Affine.translate vector v)

instance AffineTransformable (IO a) where
    transform mat iofn = preservingMatrix $ do mat' <- newMatrix RowMajor $ concat $ rowMajorForm mat
                                               multMatrix (mat' :: GLmatrix Double)
                                               iofn
    translate (Vector3D x y z) iofn = preservingMatrix $ 
        do GL.translate $ Vector3 x y z
           iofn
    scale (Vector3D x y z) iofn = preservingMatrix $ 
        do GL.scale x y z
           iofn
    rotate (Vector3D x y z) angle iofn = preservingMatrix $ 
        do GL.rotate (toDegrees_ angle) (Vector3 x y z)
           iofn
\end{code}

\texttt{WrappedAffine} stores up affine transformations that are commited only when the entity is unwrapped.  This is an optimization when the entity in question is expensive
to transform.

\begin{code}
data WrappedAffine a = WrappedAffine (RSAGL.Matrix.Matrix) a

wrapAffine :: a -> WrappedAffine a
wrapAffine = WrappedAffine (identityMatrix 4)

unwrapAffine :: (AffineTransformable a) => WrappedAffine a -> a
unwrapAffine (WrappedAffine m a) = transform m a

instance AffineTransformable (WrappedAffine a) where
    transform t (WrappedAffine m a) = WrappedAffine (transform t m) a
\end{code}

\subsection{Orthagonal Systems}

\texttt{FUR} stands for Forward Up Right.  It's used to specify arbitrary orthagonal coordinate systems given any combination
of forward up and right vectors.  It also accepts down, left, and backward vectors.

\texttt{orthagonalFrame} generates an orthagonal coordinate system, such that the first vector is passed directly, the second vector
is fixed to be orthagonal to the first as by fixOrtho, and the third vector is calculated automatically to be the missing of the other two
(either forward, up, or right).

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

orthagonalFrame :: FUR Vector3D -> FUR Vector3D -> AffineTransformation
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

\texttt{modelLookAt} takes a model's position, the position or vector to forward, and the position or vector to up, 

\begin{code}
modelLookAt :: Point3D -> FUR (Either Point3D Vector3D) -> FUR (Either Point3D Vector3D) -> AffineTransformation
modelLookAt pos primaryish secondaryish = RSAGL.Affine.translate (vectorToFrom pos origin_point_3d) . orthagonalFrame primary secondary
    where primary = fmap (either (`vectorToFrom` pos) id) primaryish
          secondary = fmap (either (`vectorToFrom` pos) id) secondaryish
\end{code}