module Intrust where

import Text.Printf
import Shake.Development.FilePath

type CaseId = String

path :: String -> CaseId -> FilePath
path var siteid =
  case var of
    "fsindwi" -> printf (base </> "%s/diff/%s.fsindwi.nrrd") siteid siteid
    "dwiharm" -> printf (base </> "Harmonization-20160728/%s_hmz_iter1.nhdr") siteid
    "caselist" -> (base </> "caselist.txt")
  where base = "/data/pnl/INTRuST/"
  {-where base = "/Users/ryan/partners/data/pnl/INTRuST/"-}
