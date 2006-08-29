module PrintTextData
    (PrintTextMode(..),
     TextType(..))
    where

-- |
-- How to run printText.
--
-- Limited -- print only the most recent lines at the top of the screen
-- Unlimited -- fill the entire screen with text
-- Disabled -- don't print any text at all
--
data PrintTextMode = Limited -- print only a few lines (or only 1)
		   | Unlimited -- fill the entire screen with text
		   | Disabled -- don't print any text at all
		     deriving (Eq)

-- |
-- Type of any line of text printed on the screen.  We print the
-- different TextTypes in different colors for readability.
--
-- Untranslated -- we didn't recognize some input from roguestar-engine, so we print it in the raw
-- Information -- information from roguestar-engine
-- UserQuery -- a question put to the user
-- GUIMessage -- printed from the GUI, not the engine.
--
data TextType = Untranslated
	      | Information
	      | UserQuery
	      | GUIMessage