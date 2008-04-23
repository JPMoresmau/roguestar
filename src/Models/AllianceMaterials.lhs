\section{Alliance Materials}

\begin{code}
module Models.AllianceMaterials
    (alliance_metal)
    where

import RSAGL.Model
import RSAGL.ModelingExtras

alliance_metal :: Modeling ()
alliance_metal = 
    do pigment $ pure $ scaleRGB 0.6 gold
       specular 75 $ pure $ scaleRGB 1.0 gold
\end{code}
