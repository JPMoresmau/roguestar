module OneToMany
    where

import Data.Map as Map

data OneToMany a b c = 
    OneToMany
    (Map a [b])
    (Map b (a,c))

empty :: OneToMany a b c
empty = OneToMany (Map.empty) (Map.empty)

insert :: (a,b,c) -> OneToMany a b c -> OneToMany a b c
insert (a,b,c) (OneToMany _ bac) =
    let old_parent = lookup b bac
	clean_map = if (isNothing old_parent)
		    then bac
		    else adjust (delete b) (fst $ just old_parent)
        in OneToMany
               (insertWith (++) a [b] clean_map)
               (insert b (a,c) bac)

parent :: b -> OneToMany a b c -> Maybe a
parent b (OneToMany ab bac) = fst $ lookup b bac

children :: a -> OneToMany a b c -> Maybe [(a,b,c)]
children the_parent (OneToMany ab bac) =
    let the_children = lookup a ab
        in if (isNothing the_children)
           then []
           else zip3 (
                      repeat a,
                      the_children,
                      map (\x -> snd $ bac !! x) the_children
                     )

toList :: OneToMany a b c -> [(a,b,c)]
toList (OneToMany ab bac) = foldr1 (++) (map children (keys ab))

fromList :: [(a,b,c)] -> OneToMany a b c
fromList lst = foldr insert empty lst

