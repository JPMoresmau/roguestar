module RSAGL.Interpolation
    ()
    where

\begin{code}
-- |
-- Class of things subject to linear interpolation.
--
-- lerp u a b, with u at or between zero and unity according to its type,
-- is an entity some fraction of the distance between a b, with u=0
-- indicating a and u=1 indicating b.  If reasonable for the types
-- involved, u may be outside of the zero and unity, with a negative
-- u indicating a point on the opposite side of a from b and with
-- a u > 1 indicating a point on the opposite side of b from a.
--
class Lerpable a where
    lerp :: Double -> (a,a) -> a

instance (Lerpable a,Lerpable b) => Lerpable (a,b) where
    lerp u ((a1,a2),(b1,b2)) = (lerp u (a1,b1),lerp u (a2,b2))

genericLerp :: (Lerpable a,Real r) => r -> (a,a) -> a
genericLerp u (a,b) = lerp (realToFrac u) (a,b)

-- |
-- lerp takes a parameter between 0 and 1, while lerpBetween takes a parameter between two arbitrary values.
-- lerp u (a,b) == lerpBetween (0,u,1) (a,b)
--
lerpBetween :: (Lerpable a,Real r,Fractional r) => (r,r,r) -> (a,a) -> a
lerpBetween = lerpBetweenMutated id

lerpBetweenMutated :: (Lerpable a,Real r,Fractional r) => (Double -> Double) -> (r,r,r) -> (a,a) -> a
lerpBetweenMutated mutator (l,u,r) = lerp $ mutator $ realToFrac $ (u-l) / (r-l)

-- |
-- As lerpBetween, but constrains the parameter to the range 0 <= u <= 1.
--
lerpBetweenClamped :: (Lerpable a,Real r,Fractional r,Ord r) => (r,r,r) -> (a,a) -> a
lerpBetweenClamped = lerpBetweenClampedMutated id

lerpBetweenClampedMutated :: (Lerpable a,Real r,Fractional r,Ord r) => (Double -> Double) -> (r,r,r) -> (a,a) -> a
lerpBetweenClampedMutated mutator (l,u,r) = lerp $ max 0 $ min 1 $ mutator $ realToFrac $ (u-l) / (r-l)

-- |
-- A lerp mutator that gives a continuous 1st derivitive to the entity's change over the parameter,
-- assuming it had continuous change to begin with.
--
lerp_mutator_continuous_1st :: Double -> Double
lerp_mutator_continuous_1st x | x < 0 = 0
lerp_mutator_continuous_1st x | x > 1 = 1
lerp_mutator_continuous_1st x | x <= 0.5 = (x*2)^2 / 2
lerp_mutator_continuous_1st x = 1 - ((1-x)*2)^2 / 2

-- |
-- Given many entities, lerp between the two entities closest to the given point
-- on either side.  For example, if we wanted to lerp between colors on a rainbow,
-- we might use the map [(0,RED),(1,ORANGE),(2,YELLOW),(3,GREEN),(4,BLUE),(5,INDIGO),(6,VIOLET)].
-- lerpMap 3.5 would result in a blue-green color.
--
lerpMap :: (Lerpable a) => Double -> [(Double,a)] -> a
lerpMap u pts = 
    let (l,l') = minimumBy (\x -> \y -> compare (fst x) (fst y)) $ filter ((>= u) . fst) pts
        (r,r') = maximumBy (\x -> \y -> compare (fst x) (fst y)) $ filter ((<= u) . fst) pts
        in lerpBetween (l,u,r) (l',r')
                
lerpNumber :: (Num n) => n -> (n,n) -> n
lerpNumber u (a,b) = (1-u)*a + u*b

instance Lerpable Double where
   lerp = lerpNumber

instance Lerpable Vector3D where
   lerp u (Vector3D ax ay az,Vector3D bx by bz) = Vector3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point3D where
   lerp u (Point3D ax ay az,Point3D bx by bz) = Point3D (lerp u (ax,bx)) (lerp u (ay,by)) (lerp u (az,bz))

instance Lerpable Point2D where
   lerp u (Point2D ax ay,Point2D bx by) = Point2D (lerp u (ax,bx)) (lerp u (ay,by))
                
instance (Lerpable a) => Lerpable (Maybe a) where
   lerp u (a,b) = liftM2 (curry $ lerp u) a b
\end{code}

\begin{code}
interpolateBetween3d :: [Point3D] -> [Point3D]
interpolateBetween3d pts | length pts < 4 = head pts :
					    (concatMap 
					     (\(p1,p2) -> [lerp (0.5 :: Double) (p1,p2),p2]) $ 
					    doubles pts)
interpolateBetween3d pts = let len = length pts
			       begin = take 3 pts
			       end = drop (len-2) pts
			       in (take 3 $ interpolateBetween3d begin) ++ 
				      (concatMap (\x -> [goodInterpolateFn x,x !! 2]) $ consecutives 4 pts) ++ 
				      (drop 2 $ interpolateBetween3d end)

loopedInterpolateBetween3d :: [Point3D] -> [Point3D]
loopedInterpolateBetween3d pts | length pts < 4 = (concatMap
						   (\(p1,p2) -> [lerp (0.5 :: Double) (p1,p2),p2]) $
						  loopedDoubles pts)
loopedInterpolateBetween3d pts = (concatMap (\x -> [goodInterpolateFn x,x !! 2]) $ loopedConsecutives 4 pts)

interpolateBetween2d :: [Point2D] -> [Point2D]
interpolateBetween2d = map to2d . interpolateBetween3d . map to3d

goodInterpolateFn :: [Point3D] -> Point3D
goodInterpolateFn [p0,p1,p2,p3] = 
    let (Point3D x y z) = lerp (0.5 :: Double) (p1,p2)
	simple = vectorAdd (vectorToFrom p1 p0) (vectorToFrom p2 p3)
	scaled = maybe (Vector3D 0 0 0) (vectorScaleTo (vectorLength (vectorToFrom p1 p2) / 9)) $ aNonZeroVector simple
	(Vector3D x' y' z') = if (vectorLength simple < vectorLength scaled)
			      then simple
			      else scaled
	in Point3D (x+x'/2) (y+y'/2) (z+z'/2)
goodInterpolateFn _ = error "goodInterpolateFn: works only on lists of 4"
\end{code}