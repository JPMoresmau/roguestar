\section{Wrapped Affine Data Types}

\begin{code}
module RSAGL.WrappedAffine
    (WrappedAffine(..),
     wrapAffine,
     unwrapAffine)
    where

import RSAGL.Affine
import RSAGL.CoordinateSystems
\end{code}

\texttt{WrappedAffine} stores up affine transformations that are commited only when the entity is unwrapped.  In this way we can store affine transformations for entities that can not be directly transformed, or for which delaying transformation as long as possible is an optimization.

\begin{code}
data WrappedAffine a = WrappedAffine CoordinateSystem a

wrapAffine :: a -> WrappedAffine a
wrapAffine = WrappedAffine root_coordinate_system

unwrapAffine :: (AffineTransformable a) => WrappedAffine a -> a
unwrapAffine (WrappedAffine cs a) = migrateToFrom root_coordinate_system cs a

instance AffineTransformable (WrappedAffine a) where
    transform t (WrappedAffine cs a) = WrappedAffine (transform t cs) a

instance Functor WrappedAffine where
    fmap f (WrappedAffine m a) = WrappedAffine m $ f a
\end{code}
