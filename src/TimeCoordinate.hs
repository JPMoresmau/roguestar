--
-- Right now this is a simple abstraction over Rational.
-- In the future, when we implement time travel, it will be more interesting.
--
-- 1 = 1 minute, or 1 round.  Nothing should ever take longer than a round.
-- A typical creature should be able to move about 20 squares in a round.
-- Weapons should do ideal damage at a standard rate of 100 points of damage per round.
-- Creatures should heal their entire hit points in exactly one round, after not taking any damage for one round.
--

module TimeCoordinate
    (TimeCoordinate,
     advanceTime,
     zero_time)
    where

data TimeCoordinate = TimeCoordinate Rational
    deriving (Eq,Ord,Read,Show)

advanceTime :: Rational -> TimeCoordinate -> TimeCoordinate
advanceTime x (TimeCoordinate t) = TimeCoordinate (t + x)

zero_time :: TimeCoordinate
zero_time = TimeCoordinate 0
