\section{Recreant Materials}

\begin{code}
module Models.RecreantMaterials
    (recreant_metal)
    where

import RSAGL.Model
import RSAGL.ModelingExtras

recreant_material :: Modeling ()
recreant_material = 
    do pigment $ pure camouflage_green 
       specular 25 $ pure camouflage_green
\end{code}
