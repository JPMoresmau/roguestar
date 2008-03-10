\section{Encephalon Materials}

\begin{code}
module Models.EncephalonMaterials
    (encephalon_skin)
    where

import RSAGL.ModelingExtras
import RSAGL.Model

encephalon_skin :: Modeling ()
encephalon_skin = pigment $ pattern (cloudy 32 0.01) [(0.0,pure olive),(1.0,pure coral)]
\end{code}
