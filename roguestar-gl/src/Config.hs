module Config
    (short_version_string,
     version_string,
     window_name,
     default_window_size)
    where

import Paths_roguestar_gl
import Data.List
import Data.Version

short_version_string :: String
short_version_string =
    (concat $ intersperse "." $ map show $ versionBranch version)

version_string :: String
version_string = short_version_string ++ if null (versionTags version)
                     then "" else "[" ++ unwords tags_str ++ "]"
    where tags_str = intersperse ", " $ map show $ versionTags version

window_name :: String
window_name = "Roguestar-GL " ++ version_string

default_window_size :: (Integral i) => (i,i)
default_window_size = (800,600)

