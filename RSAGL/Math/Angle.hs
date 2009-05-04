{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module RSAGL.Math.Angle
    (Angle,
     BoundAngle(..),
     fromDegrees,
     fromRadians,
     fromRotations,
     fromTimeOfDayHMS,
     fromArcMinutes,
     fromArcSeconds,
     sine,
     arcSine,
     cosine,
     arcCosine,
     tangent,
     arcTangent,
     cartesianToPolar,
     polarToCartesian,
     toRadians,
     toRadians_,
     toDegrees,
     toDegrees_,
     toRotations,
     toRotations_,
     scaleAngle,
     supplementaryAngle,
     zero_angle,
     angularIncrements,
     angleAdd,
     angleSubtract,
     angleNegate,
     absoluteAngle,
     unboundAngle)
    where

import RSAGL.Math.FMod
import RSAGL.Math.AbstractVector

-- | An angular value.
newtype Angle = Radians Double deriving (Show)

-- | An angular value.  'BoundAngle's are always in the range between -180 and 180 degrees, inclusive.
newtype BoundAngle = BoundAngle Angle deriving (Show)

zero_angle :: Angle
zero_angle = Radians 0

instance Eq Angle where
    (==) a b = case (toRadians_ a,toRadians_ b) of
                   (x,y) | abs x == pi && abs y == pi -> True
                   (x,y) | x == y -> True
                   _ -> False

instance Ord Angle where
    compare x y = case () of
                      _ | x == y -> EQ
                      _ -> compare (toRadians_ x) (toRadians_ y)

instance AbstractZero Angle where
    zero = zero_angle

instance AbstractZero BoundAngle where
    zero = BoundAngle zero_angle

instance AbstractAdd Angle Angle where
    add = angleAdd

instance AbstractAdd BoundAngle Angle where
    add (BoundAngle a) x = BoundAngle $ boundAngle $ a `add` x

instance AbstractSubtract Angle Angle where
    sub = angleSubtract

instance AbstractSubtract BoundAngle Angle where
    sub (BoundAngle a) (BoundAngle b) = boundAngle $ a `sub` b

instance AbstractScale Angle where
    scalarMultiply = scaleAngle

instance AbstractVector Angle

instance AbstractMagnitude Angle where
    magnitude = toRotations_ . absoluteAngle

-- | angularIncrements answers n evenly distributed angles from 0 to 2*pi.
angularIncrements :: Integer -> [Angle]
angularIncrements subdivisions = map (fromRadians . (2*pi*) . (/ fromInteger subdivisions) . fromInteger) [0 .. subdivisions - 1]

-- | There are 2*pi radians in a circle.
fromRadians :: Double -> Angle
fromRadians = Radians

-- | There are 260 degrees in a circle.
fromDegrees :: Double -> Angle
fromDegrees = Radians . ((*) (pi/180))

-- | There is 1 rotation in a circle.
fromRotations :: Double -> Angle
fromRotations = Radians . ((*) (2*pi))

-- | Get an angle based on time of day, hours, minutes, seconds, where noon is considered a zero angle.
fromTimeOfDayHMS :: Double -> Double -> Double -> Angle
fromTimeOfDayHMS h m s = fromRotations (((s/60+m)/60+h)/24)

-- | There are 21600 arc minutes in a circle, 60 arc minutes in a degree.
fromArcMinutes :: Double -> Angle
fromArcMinutes = fromDegrees . (/60)

-- | There are 1296000 arc seconds in a circle, 60 arc seconds in an arc minutes.
fromArcSeconds :: Double -> Angle
fromArcSeconds = fromArcMinutes . (/60)

-- | Answers the angle in the range of -180 to 180, inclusive.
toDegrees :: Angle -> Double
toDegrees x = let x' = toRadians x
                  in x' * 180 / pi


-- | 'toDegrees_' answers the angle in degrees with no range limitation.
toDegrees_ :: Angle -> Double
toDegrees_ (Radians x) = x * 180 / pi

-- | 'toRadians' answers the angle in the range of -pi .. pi, inclusive.
toRadians :: Angle -> Double
toRadians x = let (Radians x') = boundAngle x
                  in x'


-- | toRadians answers the angle in radians with no range limitation.
toRadians_ :: Angle -> Double
toRadians_ (Radians x) = x

-- | 'toRotations' answers the angle in the range of -0.5 to 0.5, inclusive.
toRotations :: Angle -> Double
toRotations x= let x' = toRadians x
                   in x' / pi / 2


-- | 'toRotations' answers the angle in rotations with no range limitation.
toRotations_ :: Angle -> Double
toRotations_ (Radians x) = x / pi / 2

scaleAngle :: Double -> Angle -> Angle
scaleAngle x = Radians . (*x) . toRadians_

supplementaryAngle :: Angle -> Angle
supplementaryAngle (Radians x) = Radians $ pi - x

angleAdd :: Angle -> Angle -> Angle
angleAdd (Radians x) (Radians y) = Radians $ x + y

angleSubtract :: Angle -> Angle -> Angle
angleSubtract (Radians x) (Radians y) = Radians $ x - y

angleNegate :: Angle -> Angle
angleNegate (Radians x) = Radians $ negate x

-- | Absolute value of an angle.
absoluteAngle :: Angle -> Angle
absoluteAngle (Radians x) = Radians $ abs x

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

cartesianToPolar :: (Double,Double) -> (Angle,Double)
cartesianToPolar (u,v) = (fromRadians $ atan2 v u,sqrt $ u*u + v*v)

polarToCartesian :: (Angle,Double) -> (Double,Double)
polarToCartesian (a,d) = (cosine a*d,sine a*d)

-- | 'boundAngle' forces the angle into the range (-pi..pi).
boundAngle :: Angle -> Angle
boundAngle (Radians x) = Radians $ if bounded > pi then bounded - 2*pi else bounded
    where bounded = x `fmod` (2*pi)

unboundAngle :: BoundAngle -> Angle
unboundAngle (BoundAngle a) = a
