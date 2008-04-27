\section{Coordinate System Neutral Data}

Coordinate system neutral (\texttt{CSN}) data can transparently be imported into or exported from any affine coordinate system.  All \texttt{AffineTransformable} entities
can be represented in coordinate system neutral form.

\begin{code}

{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.CoordinateSystems
    (AffineTransformation,
     affine_identity,
     CoordinateSystem,
     Affine(..),
     affineOf,
     CoordinateSystemClass(..),
     NestedCoordinateSystemTransformer,
     root_coordinate_system,
     migrate,
     transformation,
     inverseTransformation,
     CSN,
     importCSN,
     exportCSN,
     remoteCSN,
     importM,
     exportM,
     remoteM,
     importA,
     importFromA,
     exportA,
     exportToA,
     exportCoordinateSystem,
     remoteA,
     transformM,
     transformA,
     Distance,
     measure,
     distance,
     distanceSquared)
    where

import Control.Arrow
import Control.Monad.State
import Control.Arrow.Operations
import RSAGL.Matrix
import RSAGL.Affine
import RSAGL.Vector
\end{code}

\subsection{Coordinate Systems}

A \texttt{CoordinateSystem} is the context by which coordinate system neutral data can be imported or exported.

\texttt{migrate} is the function that exports data from one coordinate system into another.

All \texttt{CoordinateSystems} are affine transformations of the \texttt{root_coordinate_system}.

\begin{code}
data CoordinateSystem = CoordinateSystem Matrix deriving (Show)

instance AffineTransformable CoordinateSystem where
    transform m (CoordinateSystem cs) = CoordinateSystem $ matrixMultiply m cs

migrate :: (AffineTransformable a) => CoordinateSystem -> CoordinateSystem -> a -> a
migrate (CoordinateSystem from) (CoordinateSystem to) = inverseTransform to . transform from

class CoordinateSystemClass csc where
    getCoordinateSystem :: csc -> CoordinateSystem
    storeCoordinateSystem :: CoordinateSystem -> csc -> csc

instance CoordinateSystemClass CoordinateSystem where
    getCoordinateSystem = id
    storeCoordinateSystem cs = const cs

instance (CoordinateSystemClass csc) => CoordinateSystemClass (a,csc) where
    getCoordinateSystem = getCoordinateSystem . snd
    storeCoordinateSystem cs = second (storeCoordinateSystem cs)

root_coordinate_system :: CoordinateSystem
root_coordinate_system = CoordinateSystem $ identityMatrix 4
\end{code}

\subsection{Abstract Affine Transformations}

\begin{code}
newtype Affine = Affine { affine_transformation :: forall a. AffineTransformable a => a -> a }
type AffineTransformation = Affine -> Affine

instance AffineTransformable Affine where
   transform m (Affine f) = Affine $ transform m . f

affine_identity :: AffineTransformation
affine_identity = id

affineOf :: AffineTransformation -> Affine
affineOf = ($ (Affine id))

affineTransformationToMatrix :: AffineTransformation -> Matrix
affineTransformationToMatrix f = affine_transformation (affineOf f) $ identityMatrix 4

transformation :: (AffineTransformable a) => AffineTransformation -> a -> a
transformation = transform . affineTransformationToMatrix

inverseTransformation :: (AffineTransformable a) => AffineTransformation -> a -> a
inverseTransformation = inverseTransform . affineTransformationToMatrix

postmultiplyTransformation :: AffineTransformation -> CoordinateSystem -> CoordinateSystem
postmultiplyTransformation f (CoordinateSystem cs) = CoordinateSystem $ cs `matrixMultiply` affineTransformationToMatrix f
\end{code}

\subsection{Coordinate System Neutral Data}

\texttt{exportCSN} exports any \texttt{AffineTransformable} data structure.  \texttt{importCSN} imports data into the local coordinate system.

\texttt{remoteCSN} operates as a functor to perform non-affine transformations over coordinate system neutral data.  Since any such function
is potentially non-affine, it must take place within the context of a \texttt{CoordinateSystem}.

Versions of each of these functions are defined for state monads and state arrows, where the state type implements \texttt{CoordinateSystemClass}.
\begin{code}
data CSN a = CSN a deriving (Show)

exportCSN :: (AffineTransformable a) => CoordinateSystem -> a -> CSN a
exportCSN (CoordinateSystem m) a = CSN $ transform m a

importCSN :: (AffineTransformable a) => CoordinateSystem -> CSN a -> a
importCSN (CoordinateSystem m) (CSN a) = inverseTransform m a

remoteCSN :: (AffineTransformable a,AffineTransformable b) => CoordinateSystem -> (a -> b) -> CSN a -> CSN b
remoteCSN context f = exportCSN context . f . importCSN context

exportM :: (Monad m,MonadState s m,CoordinateSystemClass s,AffineTransformable a) => a -> m (CSN a)
exportM a = liftM (flip exportCSN a) $ gets getCoordinateSystem

importM :: (Monad m,MonadState s m,CoordinateSystemClass s,AffineTransformable a) => CSN a -> m a
importM a = liftM (flip importCSN a) $ gets getCoordinateSystem

remoteM :: (Monad m,MonadState s m,CoordinateSystemClass s,AffineTransformable a,AffineTransformable b) => CoordinateSystem -> (a -> b) -> a -> m b
remoteM context f a = 
    do b <- liftM (remoteCSN context f) $ exportM a
       importM b

exportA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a) => arr a (CSN a)
exportA = proc a ->
    do cs <- arr getCoordinateSystem <<< fetch -< ()
       returnA -< exportCSN cs a

exportToA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a) => CoordinateSystem -> arr a a
exportToA cs = exportA >>> arr (importCSN cs)

exportCoordinateSystem :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s) => arr AffineTransformation CoordinateSystem
exportCoordinateSystem = exportToA root_coordinate_system <<< arr (flip transformation root_coordinate_system)

importA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a) => arr (CSN a) a
importA = proc a ->
    do cs <- arr getCoordinateSystem <<< fetch -< ()
       returnA -< importCSN cs a

importFromA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a) => CoordinateSystem -> arr a a
importFromA cs = arr (exportCSN cs) >>> importA

remoteA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,AffineTransformable a,AffineTransformable b) => arr (CoordinateSystem, (a -> b), a) b
remoteA = proc (context,f,a) ->
    do csn <- exportA -< a
       importA -< remoteCSN context f csn
\end{code}

\subsection{Affine Transformation in State Monads and State Arrows}

\begin{code}
class NestedCoordinateSystemTransformer a where
    transformCoordinateSystem :: a -> CoordinateSystem -> CoordinateSystem

instance NestedCoordinateSystemTransformer Affine where
    transformCoordinateSystem (Affine f) = postmultiplyTransformation f

instance NestedCoordinateSystemTransformer CoordinateSystem where
    transformCoordinateSystem cs = const cs

transformM :: (Monad m,MonadState s m,CoordinateSystemClass s,
               NestedCoordinateSystemTransformer cst) => cst -> m a -> m a
transformM ncst action =
    do s <- liftM getCoordinateSystem get
       modify (storeCoordinateSystem (transformCoordinateSystem ncst s))
       a <- action
       modify (storeCoordinateSystem s)
       return a

transformA :: (Arrow arr,ArrowState s arr,CoordinateSystemClass s,
               NestedCoordinateSystemTransformer cst) => arr a b -> arr (cst,a) b
transformA action = proc (ncst,a) ->
    do s <- fetch -< ()
       store -< storeCoordinateSystem (transformCoordinateSystem ncst $ getCoordinateSystem s) s
       b <- action -< a
       s' <- fetch -< ()
       store -< storeCoordinateSystem (getCoordinateSystem s) s'
       returnA -< b
\end{code}

\subsection{Coordinate System Neutral Distance}

Since we can't make scalar values \texttt{AffineTransformable}, but it is useful to measure distances in a space that is subject to affine transformations,
we define distance in terms of the elements being measured.

\begin{code}
data Distance = forall p. (AffineTransformable p,Xyz p) => Distance p p

measure :: (AffineTransformable p,Xyz p) => p -> p -> Distance
measure = Distance

distance :: Distance -> Double
distance (Distance p1 p2) = distanceBetween p1 p2

distanceSquared :: Distance -> Double
distanceSquared (Distance p1 p2) = distanceBetweenSquared p1 p2

instance AffineTransformable Distance where
    transform m (Distance p1 p2) = Distance (transform m p1) (transform m p2)
    scale v (Distance p1 p2) = Distance (scale v p1) (scale v p2)
    rotate a v (Distance p1 p2) = Distance (rotate a v p1) (rotate a v p2)
    translate v (Distance p1 p2) = Distance (translate v p1) (translate v p2)
\end{code}
