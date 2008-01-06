\section{RSAGL.Angle}

RSAGL.Angle supports manipulation of angular values, including
angular arithmetic and trigonometry.

\begin{code}
module RSAGL.Angle
    (Angle,
     fromDegrees,
     fromRadians,
     sine,
     arcSine,
     cosine,
     arcCosine,
     tangent,
     arcTangent,
     toRadians,
     toRadians_,
     toDegrees,
     toDegrees_,
     scaleAngle,
     zero_angle,
     angularIncrements,
     angleAdd,
     angleSubtract,
     angleNegate,
     absoluteAngle)
    where

import Data.Fixed
import RSAGL.AbstractVector

newtype Angle = Radians Double deriving (Show)

zero_angle :: Angle
zero_angle = Radians 0

instance Eq Angle where
    (==) a b = case (toRadians a,toRadians b) of
                   (x,y) | abs x == pi && abs y == pi -> True
                   (x,y) | x == y -> True
                   _ -> False

instance Ord Angle where
    compare x y = case () of
                      _ | x == y -> EQ
                      _ -> compare (toRadians x) (toRadians y)

instance AbstractVector Angle where
    zero = zero_angle
    add = angleAdd
    sub = angleSubtract
    scalarMultiply = scaleAngle
\end{code}

angularIncrements answers n equa-angular values from 0 to 2*pi.

\begin{code}
angularIncrements :: Integer -> [Angle]
angularIncrements subdivisions = map (fromRadians . (2*pi*) . (/ fromInteger subdivisions) . fromInteger) [0 .. subdivisions - 1]
\end{code}

\subsection{Type coercion for Angles}

\begin{code}
fromRadians :: Double -> Angle
fromRadians = Radians

fromDegrees :: Double -> Angle
fromDegrees = Radians . ((*) (pi/180))
\end{code}

\texttt{toDegrees} answers the angle in the range of -180 to 180, inclusive.

\texttt{toDegrees\_} answers the angle in degrees with no range limitation.

\begin{code}
toDegrees :: Angle -> Double
toDegrees x = let x' = toRadians x
                  in x' * 180 / pi

toDegrees_ :: Angle -> Double
toDegrees_ (Radians x) = x * 180 / pi
\end{code}

\texttt{toRadians} answers the angle in the range of -pi .. pi, inclusive.

\texttt{toRadians\_} answers the angle in radians with no range limitation.

\begin{code}
toRadians :: Angle -> Double
toRadians x = let (Radians x') = toBoundedAngle x
                  in x'

toRadians_ :: Angle -> Double
toRadians_ (Radians x) = x
\end{code}

\subsection{Manipulating Angular values}

\begin{code}
scaleAngle :: Double -> Angle -> Angle
scaleAngle x = Radians . (*x) . toRadians

angleAdd :: Angle -> Angle -> Angle
angleAdd (Radians x) (Radians y) = Radians $ x + y

angleSubtract :: Angle -> Angle -> Angle
angleSubtract (Radians x) (Radians y) = toBoundedAngle $ Radians $ x - y

angleNegate :: Angle -> Angle
angleNegate (Radians x) = Radians $ negate x

absoluteAngle :: Angle -> Angle
absoluteAngle a = let (Radians x) = toBoundedAngle a in Radians $ abs x

sine :: Angle -> Double
sine (Radians x) = sin x

arcSine :: Double -> Angle
arcSine = fromRadians . asin

cosine :: Angle -> Double
cosine (Radians x) = cos x

arcCosine :: Double -> Angle
arcCosine = fromRadians . acos

tangent :: Angle -> Double
tangent (Radians x) = tan x

arcTangent :: Double -> Angle
arcTangent = fromRadians . atan
\end{code}

\texttt{toBoundedAngle} forces the angle into the range (-pi..pi).

\begin{code}
toBoundedAngle :: Angle -> Angle
toBoundedAngle (Radians x) = Radians $ if bounded > pi then bounded - 2*pi else bounded
    where bounded = x `mod'` (2*pi)
\end{code}
