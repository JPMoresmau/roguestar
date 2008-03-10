\section{PrintText Data Structures}

\begin{code}
module PrintTextData
    (PrintTextMode(..),
     TextType(..))
    where
\end{code}
How to run printText.

Limited -- print only the most recent lines at the top of the screen
Unlimited -- fill the entire screen with text
Disabled -- don't print any text at all

\begin{code}
data PrintTextMode = Limited -- print only a few lines (or only 1)
		   | Unlimited -- fill the entire screen with text
		   | Disabled -- don't print any text at all
		     deriving (Eq)
\end{code}

Type of any line of text printed on the screen.  We print the
different TextTypes in different colors for readability.

\begin{code}
data TextType = Event -- message related to an even in the game 
	      | UnexpectedEvent -- 
	      | Input -- something the user typed in
	      | Query
\end{code}
