module Intrust 
    (path
    )
    where

import Text.Printf
import Development.Shake
import Development.Shake.FilePath

path :: String -> String -> FilePath
path var siteid =
  case var of
    "fsindwi" -> printf (base </> "%s/diff/%s.fsindwi.nrrd") siteid siteid
    "dwiharm" -> printf (base </> "Harmonization-20160728/%s_hmz_iter1.nhdr") siteid
    "dwied" -> printf (base </> "%s/diff/%s-dwi-Ed.nhdr") siteid siteid
    _ -> error "Add this var to Intrust.hs"
  where base = "/data/pnl/INTRuST/"
  {-where base = "/Users/ryan/partners/data/pnl/INTRuST/"-}
