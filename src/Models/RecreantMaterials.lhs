\section{Recreant Materials}

\begin{code}
module Models.RecreantMaterials
    (recreant_metal)
    where

import RSAGL.Model
import RSAGL.ModelingExtras

recreant_metal :: Modeling ()
recreant_metal = 
    do pigment $ pure camouflage_green 
       specular 25 $ pure white
\end{code}
