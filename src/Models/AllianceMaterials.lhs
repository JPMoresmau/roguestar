\section{Alliance Materials}

\begin{code}
module Models.AllianceMaterials
    (alliance_metal)
    where

import RSAGL.Model
import RSAGL.ModelingExtras

alliance_metal :: Modeling ()
alliance_metal = 
    do pigment $ pure gold
       specular 75 $ pure gold
\end{code}
