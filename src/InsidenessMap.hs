module OneToMany
    where

data OneToMany a b c = 
    OneToMany
    (Map a [b])
    (Map b (a,c))

otmPut :: (a,b,c) -> OnetoMany a b c -> OneToMany a b c
otmPut (a,b,c) (OneToMany ab bac) =
    let old_parent = lookup b bac
	clean_map = if (isNothing old_parent)
		    then bac
		    else adjust (delete b) (fst $ just old_parent)
    OneToMany
    (insertWith (++) a [b] clean_map)
    (insert b (a,c) bac)