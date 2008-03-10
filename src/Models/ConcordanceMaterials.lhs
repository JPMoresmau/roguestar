\section{Condorance Materials}

\begin{code}
module Models.ConcordanceMaterials
    (concordance_metal)
    where

import RSAGL.Model
import RSAGL.ModelingExtras

concordance_metal :: Modeling ()
concordance_metal =
    do pigment $ pure slate_gray
       specular 45$ pure lilac
\end{code}
