{-# OPTIONS_GHC -farrows #-}

module RSAGL.TestFunctions
    (addFive)
    where

import RSAGL.StatefulArrow
import Control.Arrow.Operations

countingArrow :: Integer -> StatefulFunction Integer Integer
countingArrow = internally $ 
                    proc x -> do y <- fetch -< ()
                                 store -< x + y
                                 fetch -< ()

addFive :: Integer -> Integer
addFive x = let (_,sf1) = runStatefulArrow (countingArrow x) 1
                (_,sf2) = runStatefulArrow sf1 1
                (_,sf3) = runStatefulArrow sf2 1
                (_,sf4) = runStatefulArrow sf3 1
                in fst $ runStatefulArrow sf4 1