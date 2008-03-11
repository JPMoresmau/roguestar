\section{Strings}

This section merely contains a mapping from strings used in the protocol to human-readable strings.
For example, "str" becomes "strength."

\begin{code}
module Strings
    (hrstring)
    where

hrstring :: String -> String
hrstring "str" = "strength"
hrstring "dex" = "dexterity"
hrstring "con" = "constitution"
hrstring "int" = "intelligence"
hrstring "per" = "perception"
hrstring "cha" = "charisma"
hrstring "mind" = "mindfulness"
hrstring "forceadept" = "force adept"
hrstring x = x
\end{code}
