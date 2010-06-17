module PrintTextData
    (PrintTextMode(..),
     TextType(..))
    where

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
