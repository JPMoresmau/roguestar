{-# LANGUAGE OverloadedStrings #-}

module PrintTextData
    (PrintTextMode(..),
     TextType(..),
     StatusField(..),
     onChange,
     whileActive)
    where

import qualified Data.ByteString.Char8 as B

-- | How much of the screen should we use for text.
data PrintTextMode = Limited -- ^ print only a few lines
                   | Unlimited -- ^ fill the entire screen with text
                   | Disabled -- ^ don't print any text at all
                     deriving (Eq)

-- |
-- Type of any line of text printed on the screen.  We print the
-- different TextTypes in different colors for readability.
--
data TextType = Event -- ^ message related to an event in the game
              | UnexpectedEvent -- ^ message related to a software problem
              | Input -- ^ something the user typed in
              | Query -- ^ asking the user to type something
              | Update -- ^ a change in an ongoing status condition

data StatusField =
    PlanetName
  | CompassHeading
  | DungeonDepth
        deriving (Eq,Ord)

-- | Message to print when a piece of status information changes.
onChange :: StatusField -> B.ByteString -> B.ByteString
onChange PlanetName s = "Welcome to " `B.append` s `B.append` "."
onChange CompassHeading s = "Your compass is now pointing " `B.append` s `B.append` "."
onChange DungeonDepth s = "You are now on dungeon level: " `B.append` s `B.append` "."

-- | Continuous status messages.
whileActive :: StatusField -> B.ByteString -> B.ByteString
whileActive PlanetName s =     "Planet:  " `B.append` s
whileActive CompassHeading s = "Compass: " `B.append` s
whileActive DungeonDepth s =   "Depth:   " `B.append` s

