\section{Materials}

\begin{code}
module Models.Materials
    (alliance_metal,
     concordance_metal,
     concordance_dark_glass,
     concordance_bright_glass,
     caduceator_skin,
     encephalon_skin,
     recreant_metal)
    where

import RSAGL.Model
import RSAGL.ModelingExtras
\end{code}

\subsection{Materials by Alliance}

\subsubsection{Alliance Materials}

\begin{code}
alliance_metal :: Modeling ()
alliance_metal = 
    do pigment $ pure $ scaleRGB 0.6 gold
       specular 75 $ pure $ scaleRGB 1.0 gold
\end{code}

\subsubsection{Condorance Materials}

\begin{code}
concordance_metal :: Modeling ()
concordance_metal =
    do pigment $ pure slate_gray
       specular 45 $ pure lilac
\end{code}

\begin{code}
concordance_dark_glass :: Modeling ()
concordance_dark_glass =
    do pigment $ pure black
       specular 85 $ pure eggplant
\end{code}

\begin{code}
concordance_bright_glass :: Modeling ()
concordance_bright_glass =
    do pigment $ pure black
       emissive $ pure puce
       specular 85 $ pure eggplant
\end{code}

\section{Materials by Species}

\subsubsection{Encephalon Materials}

\begin{code}
caduceator_skin :: Modeling ()
caduceator_skin = pigment $ pattern (cloudy 75 0.01) [(0.0,pure red),(0.5,pure safety_orange),(1.0,pure black)]
\end{code}

\begin{code}
encephalon_skin :: Modeling ()
encephalon_skin = pigment $ pattern (cloudy 32 0.01) [(0.0,pure firebrick),(1.0,pure coral)]
\end{code}

\subsubsection{Recreant Materials}

\begin{code}
recreant_metal :: Modeling ()
recreant_metal = 
    do pigment $ pure camouflage_green 
       specular 25 $ pure white
\end{code}

