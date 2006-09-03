module DefaultKeymap
    (default_keymap)
    where

-- |
-- Ensure that keymap entries containing a string longer than 1 end with a return character ('\n').
--
filterKeymap :: [(String,String)] -> [(String,String)]
filterKeymap keymap = concatMap (\(x,y) -> if (length x > 1) 
		                           then [((unwords $ words x) ++ "\r",y),((unwords $ words x) ++ "\n",y)] 
					   else [(x,y)])
					   keymap

default_keymap :: [(String,String)]
default_keymap = filterKeymap
		 [("x","anachronid"),
		  ("a","androsynth"),
		  ("A","ascendant"),
		  ("c","caduceator"),
		  ("e","encephalon"),
		  ("g","goliath"),
		  ("h","hellion"),
		  ("k","kraken"),
		  ("m","myrmidon"),
		  ("p","perennial"),
		  ("r","reptilian"),
		  ("R","recreant"),
		  (".","reroll"),
		  ("b","barbarian"),
		  ("c","consular"),
		  ("e","engineer"),
		  ("a","forceadept"),
		  ("m","marine"),
		  ("n","ninja"),
		  ("p","pilot"),
		  ("P","privateer"),
		  ("s","scout"),
		  ("S","shepherd"),
		  ("t","thief"),
		  ("w","warrior")]
