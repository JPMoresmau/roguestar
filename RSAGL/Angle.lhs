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
     toDegrees,
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
fromRadians :: (Real a) => a -> Angle
fromRadians = Radians . realToFrac

fromDegrees :: (Real a) => a -> Angle
fromDegrees = Radians . ((*) (pi/180)) . realToFrac
\end{code}

toDegrees answers the angle in the range of -180 .. 180, inclusive.

\begin{code}
toDegrees :: (Fractional a,Floating a) => Angle -> a
toDegrees x = let x' = toRadians x
                  in x' * 180 / pi
\end{code}

toRadians answers the angle in the range of -pi .. pi, inclusive.

\begin{code}
toRadians :: (Fractional a) => Angle -> a
toRadians x = let (Radians x') = toBoundedAngle x
                  in realToFrac x'
\end{code}

\subsection{Manipulating Angular values}

\begin{code}
scaleAngle :: Double -> Angle -> Angle
scaleAngle x = Radians . (* x) . toRadians

sine :: Angle -> Double
sine = sin . toRadians

arcSine :: Double -> Angle
arcSine = fromRadians . asin

cosine :: Angle -> Double
cosine = cos . toRadians

arcCosine :: Double -> Angle
arcCosine = fromRadians . acos

tangent :: Angle -> Double
tangent = tan . toRadians

arcTangent :: Double -> Angle
arcTangent = fromRadians . atan
\end{code}

toBoundedAngle forces the angle into the range (-pi..pi).

\begin{code}
toBoundedAngle :: Angle -> Angle
toBoundedAngle (Radians x) | x > pi = toBoundedAngle $ Radians $ x - (2*pi)
toBoundedAngle (Radians x) | x < (-pi) = toBoundedAngle $ Radians $ x + (2*pi)
toBoundedAngle x = x
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
    (+) x y = toBoundedAngle $ Radians $ toRadians x + toRadians y
    (-) x y = toBoundedAngle $ Radians $ toRadians x - toRadians y
    (*) = error "instance Num Angle, (*): multiplication over Angles is undefined"
    abs = toBoundedAngle . Radians . abs . toRadians
    negate = toBoundedAngle . Radians . negate . toRadians
    signum = Radians . (*pi) . signum . toRadians . toBoundedAngle
    fromInteger = error "instance Num Angle, (toInteger): undefined.  You probably tried to use a literal angle.  Use fromDegrees instead."
\end{code}

