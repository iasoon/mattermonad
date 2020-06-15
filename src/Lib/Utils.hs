module Lib.Utils where

import qualified Data.Aeson as A

removePrefix :: String -> A.Options
removePrefix prefix = A.defaultOptions
    { A.fieldLabelModifier = (A.camelTo2 '_') . (dropPrefix prefix)
    }

dropPrefix :: String -> String -> String
dropPrefix [] cs = cs
dropPrefix (p:ps) (c:cs) | p == c = dropPrefix ps cs
dropPrefix _ cs = cs