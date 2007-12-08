\section{Coordinate System Neutral Data}

Coordinate system neutral (\texttt{CSN}) data can transparently be imported into or exported from any affine coordinate system.  All \texttt{AffineTransformable} entities
can be represented in coordinate system neutral form.

\begin{code}

{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.CSN
    (CoordinateSystem,
     migrate,
     CSN,
     importCSN,
     exportCSN,
     remoteCSN,
     importM,
     exportM,
     remoteM,
     importA,
     exportA,
     remoteA)
    where

import Control.Monad
import Control.Monad.State
import Control.Arrow
import Control.Arrow.Operations
import RSAGL.Matrix
import RSAGL.Affine
\end{code}

\subsection{Coordinate Systems}

A \texttt{CoordinateSystem} is the context by which coordinate system neutral data can be imported or exported.

\texttt{migrate} is the function that exports data from one coordinate system into another.

All \texttt{CoordinateSystems} are affine transformations of the \texttt{root_coordinate_system}.

\begin{code}
data CoordinateSystem = CoordinateSystem Matrix

instance AffineTransformable CoordinateSystem where
    transform m (CoordinateSystem mat) = CoordinateSystem $ transform m mat

migrate :: (AffineTransformable a) => CoordinateSystem -> CoordinateSystem -> a -> a
migrate (CoordinateSystem from) (CoordinateSystem to) = inverseTransform to . transform from

class (AffineTransformable csc) => CoordinateSystemClass csc where
    coordinateSystem :: csc -> CoordinateSystem

root_coordinate_system :: CoordinateSystem
root_coordinate_system = CoordinateSystem $ identityMatrix 4
\end{code}

\subsection{Coordinate System Neutral Data}

\texttt{exportCSN} exports any \texttt{AffineTransformable} data structure.  \texttt{importCSN} imports data into the local coordinate system.

\texttt{remoteCSN} operates as a functor to perform non-affine transformations over coordinate system neutral data.  Since any such function
is potentially non-affine, it must take place within the context of a \texttt{CoordinateSystem}.

Versions of each of these functions are defined for state monads and state arrows, where the state type implements \texttt{CoordinateSystemClass}.
\begin{code}
data CSN a = CSN a

exportCSN :: (AffineTransformable a) => CoordinateSystem -> a -> CSN a
exportCSN (CoordinateSystem m) a = CSN $ transform m a

importCSN :: (AffineTransformable a) => CoordinateSystem -> CSN a -> a
importCSN (CoordinateSystem m) (CSN a) = inverseTransform m a

remoteCSN :: (AffineTransformable a,AffineTransformable b) => CoordinateSystem -> (a -> b) -> CSN a -> CSN b
remoteCSN context f = exportCSN context . f . importCSN context

exportM :: (Monad m,MonadState s m,CoordinateSystemClass s,AffineTransformable a) => a -> m (CSN a)
exportM a = liftM (flip exportCSN a) $ gets coordinateSystem

importM :: (Monad m,MonadState s m,CoordinateSystemClass s,AffineTransformable a) => CSN a -> m a
importM a = liftM (flip importCSN a) $ gets coordinateSystem

remoteM :: (Monad m,MonadState s m,CoordinateSystemClass s,AffineTransformable a,AffineTransformable b) => CoordinateSystem -> (a -> b) -> a -> m b
remoteM context f a = 
    do b <- liftM (remoteCSN context f) $ exportM a
       importM b

exportA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a) => arr a (CSN a)
exportA = proc a ->
    do cs <- arr coordinateSystem <<< fetch -< ()
       returnA -< exportCSN cs a

importA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a) => arr (CSN a) a
importA = proc a ->
    do cs <- arr coordinateSystem <<< fetch -< ()
       returnA -< importCSN cs a

remoteA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a,AffineTransformable b) => arr (CoordinateSystem, (a -> b), a) b
remoteA = proc (context,f,a) ->
    do csn <- exportA -< a
       importA -< remoteCSN context f csn

remoteA_ :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a,AffineTransformable b) => CoordinateSystem -> (a -> b) -> arr a b
remoteA_ context f = remoteA <<< arr (\a -> (context,f,a))
\end{code}

\subsection{Affine Transformation in Monads and Arrows}

\begin{code}
transformM :: (Monad m,MonadState s m,CoordinateSystemClass s) => m a -> (forall at. AffineTransformable at => at -> at) -> m a
transformM action affine_transformation =
    do s <- get
       modify affine_transformation
       a <- action
       put s
       return a

transformA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s) => arr a b -> arr (forall at. AffineTransformable at => at -> at,a) b
transformA action = proc (affine_transformation,a) ->
    do s <- fetch -< ()
       store -< affine_transformation s
       b <- action -< a
       store -< s
       returnA -< b
\end{code}