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
     angularIncrements)
    where

newtype Angle = Radians Double deriving (Show)

zero_angle :: Angle
zero_angle = Radians 0
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

\texttt{toDegrees} answers the angle in the range of -180 .. 180, inclusive.

\texttt{toDegrees} answers the angle in degrees with no range limitation.

\begin{code}
toDegrees :: Angle -> Double
toDegrees x = let x' = toRadians x
                  in x' * 180 / pi

toDegrees_ :: Angle -> Double
toDegrees_ (Radians x) = x * 180 / pi
\end{code}

toRadians answers the angle in the range of -pi .. pi, inclusive.

toRadians_ answers the angle in radians with no range limitation.

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

toBoundedAngle forces the angle into the range (-pi..pi).

\begin{code}
toBoundedAngle :: Angle -> Angle
toBoundedAngle (Radians x) = Radians $ if bounded > pi then bounded - 2*pi else bounded
    where bounded = x - (2*pi)*(fromInteger $ floor $ x/(2*pi))
\end{code}

\subsection{Instances for angular values}

\begin{code}
instance Eq Angle where
    (==) x y = case (toRadians x,toRadians y) of
                   (x',y') | abs x' == pi && abs y' == pi -> True
                   (x',y') | x' == y' -> True
                   _ -> False

instance Ord Angle where
    compare x y = case () of
                      _ | x == y -> EQ
                      _ -> compare (toRadians x) (toRadians y)
\end{code}

signum answers -pi, 0, or pi.  negate and abs work by reflecting the angle over the vector representing the zero angle.

\begin{code}
instance Num Angle where
    (+) (Radians x) (Radians y) = Radians $ x + y
    (-) (Radians x) (Radians y) = Radians $ x - y
    (*) = error "instance Num Angle, (*): multiplication over Angles is undefined"
    abs = Radians . abs . toRadians
    negate (Radians x) = Radians (-x)
    signum = Radians . (*pi) . signum . toRadians
    fromInteger = error "instance Num Angle, (toInteger): undefined.  You probably tried to use a literal angle.  Use fromDegrees instead."
\end{code}

