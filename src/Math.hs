module Math
    (minimize,
     maximize)
    where
    
import Data.List
    
-- |
-- Find the input to a function f between l and r that results in the minimal output, accurate to + or - d.
--
-- minimize f (l,r) d
--
minimize :: (Num a,Fractional a,Ord a,Ord b) => (a -> b) -> (a,a) -> a -> a
minimize f (left,right) tolerance = minimize_ f ((left + right) / 2) (abs (left - right) / 2) tolerance

-- |
-- Find the input to a function f between l and r that results in the maximal output, accurate to + or - d.
--
-- minimize f (l,r) d
--
maximize :: (Num a,Fractional a,Ord a,Ord b,Num b) => (a -> b) -> (a,a) -> a -> a
maximize f = minimize ((* (-1)) . f)

minimize_ :: (Num a,Fractional a,Ord a,Ord b) => (a -> b) -> a -> a -> a -> a
minimize_ _ x delta tolerance | delta < tolerance = x 
minimize_ f x delta tolerance = 
    let x' = minimumBy (\l r -> compare (f l) (f r)) [x-delta,x+delta,x]  -- would be more efficient if we could memoize f
        in minimize_ f x' (delta/2) tolerance