
-- | It's useful to work with the set of coordinate systems restricted to those
-- that use orthogonal unit-scaled axes, that is, that are subject only to
-- rotation and translation.  This is because these coordinate systems are the
-- describe rigid objects.
module RSAGL.Math.Orthogonal
    (up,down,left,right,forward,backward,
     orthogonalFrame,
     modelLookAt,
     FUR)
    where

import RSAGL.Math.Affine
import RSAGL.Math.Vector
import RSAGL.Math.Matrix

-- | FUR stands for Forward Up Right.  It's used to specify arbitrary
-- orthogonal coordinate systems given any combination of forward up and right
-- vectors.  It also accepts down, left, and backward vectors.

-- | This modules uses a left-handed coordinate system.  Right is positive X,
-- up is positive Y, forward is positive Z.

-- When specifying FUR coordinate systems, the first vector is fixed
-- while the second vector will be adjusted as little as possible to guarantee
-- that it is orthogonal to the first.  The third vector never needs to be
-- specified, it can be deduced.
data FURAxis = ForwardAxis | UpAxis | RightAxis | DownAxis | LeftAxis | BackwardAxis

data FUR a = FUR FURAxis a

instance Functor FUR where
    fmap f (FUR a x) = FUR a $ f x

-- | A reference to the +Y axis.
up :: a -> FUR a
up = FUR UpAxis

-- | A reference to the -Y axis.
down :: a -> FUR a
down = FUR DownAxis

-- | A reference to the -X axis.
left :: a -> FUR a
left = FUR LeftAxis

-- | A reference to the +X axis.
right :: a -> FUR a
right = FUR RightAxis

-- | A reference to the +Z axis.
forward :: a -> FUR a
forward = FUR ForwardAxis

-- | A reference to the -Z axis.
backward :: a -> FUR a
backward = FUR BackwardAxis

-- | Combine two axial references to describe a rigid affine transformation.
-- Accepts any combination of non-coaxial references.
-- In the affine transformation, the old axes will be mapped onto the specified
-- freeform axes.
--
-- The first parameter is absolute, meaning that the source axis will always map
-- perfectly onto the destination axis.  The second parameter will be obeyed
-- on a "best effort" basis.
--
orthogonalFrame :: (AffineTransformable a) => FUR Vector3D -> FUR Vector3D -> a -> a
orthogonalFrame (FUR ForwardAxis f) (FUR RightAxis r) =
    let (r',u') = fixOrtho2 f r in transform (xyzMatrix r' u' (vectorNormalize f))
orthogonalFrame (FUR UpAxis u) (FUR ForwardAxis f) =
    let (f',r') = fixOrtho2 u f in transform (xyzMatrix r' (vectorNormalize u) f')
orthogonalFrame (FUR RightAxis r) (FUR UpAxis u) =
    let (u',f') = fixOrtho2 r u in transform (xyzMatrix (vectorNormalize r) u' f')
orthogonalFrame (FUR RightAxis r) (FUR ForwardAxis f) =
    let (f',u') = fixOrtho2Left r f in transform (xyzMatrix (vectorNormalize r) u' f')
orthogonalFrame (FUR ForwardAxis f) (FUR UpAxis u) =
    let (u',r') = fixOrtho2Left f u in transform (xyzMatrix r' u' (vectorNormalize f))
orthogonalFrame (FUR UpAxis u) (FUR RightAxis r) =
    let (r',f') = fixOrtho2Left u r in transform (xyzMatrix r' (vectorNormalize u) f')
orthogonalFrame (FUR ForwardAxis _) (FUR ForwardAxis _) =
    error "orthogonalFrame: two forward vectors"
orthogonalFrame (FUR UpAxis _) (FUR UpAxis _) =
    error "orthogonalFrame: two up vectors"
orthogonalFrame (FUR RightAxis _) (FUR RightAxis _) =
    error "orthogonalFrame: two right vectors"
orthogonalFrame x y = orthogonalFrame (furCorrect x) (furCorrect y)

-- | Function to transform down, left, or backward axes into
-- up, right, or forward axes.
furCorrect :: FUR Vector3D -> FUR Vector3D
furCorrect (FUR ForwardAxis f) = FUR ForwardAxis f
furCorrect (FUR UpAxis u) = FUR UpAxis u
furCorrect (FUR RightAxis r) = FUR RightAxis r
furCorrect (FUR DownAxis d) = FUR UpAxis $ vectorScale (-1) d
furCorrect (FUR LeftAxis l) = FUR RightAxis $ vectorScale (-1) l
furCorrect (FUR BackwardAxis b) = FUR ForwardAxis $ vectorScale (-1) b

-- | Translates and rotates a model to aim at a given position or in a
-- given direction from a given vantage point.  This is analogous
-- to camera look-at functions, and could be used, for example, to
-- cause a model of an eyeball to track a particular target.
-- The first parameter is the position of the model.  Typically the second
-- parameter will be the position of the target, and the third parameter will
-- @(up \$ Vector3D 0 1 0)@.
modelLookAt :: (AffineTransformable a) =>
    Point3D ->
    FUR (Either Point3D Vector3D) ->
    FUR (Either Point3D Vector3D) ->
    a -> a
modelLookAt pos primaryish secondaryish =
          RSAGL.Math.Affine.translate (vectorToFrom pos origin_point_3d) .
              orthogonalFrame primary secondary
    where primary = fmap (either (`vectorToFrom` pos) id) primaryish
          secondary = fmap (either (`vectorToFrom` pos) id) secondaryish

