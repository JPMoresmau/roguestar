module RSAGL.Color.Alpha
    (Alpha(..))
    where

import RSAGL.Types

-- | A color with an alpha channel.
data Alpha c = Alpha {
    alpha_alpha :: {-# UNPACK #-} !RSdouble,
    alpha_color :: {-# UNPACK #-} !c }

instance Functor Alpha where
    fmap f (Alpha a c) = Alpha a $ f c

