module Nrrd
  (mask
  ,fa
  ,gzip
  ,makeMask
  )
  where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Command

gzip :: FilePath -> Action ()
gzip out = command_ [] "unu" ["save","-e","gzip","-f","nrrd","-i",out,"-o",out]

mask :: FilePath -> FilePath -> FilePath -> Action ()
mask mask vol out = do
    unit $ cmd "unu 3op ifelse" mask vol "0" "-o" out
    gzip out

fa :: FilePath -> FilePath -> Action ()
fa dwi out = do
  withTempFile $ \tensor -> do
    command_ [] "tend" ["estim","-est","lls","-B","kvp","-knownB0","true","-i",dwi,"-o",tensor]
    command_ [] "tend" ["anvol","-t","-1","-a","fa","-i",tensor,"-o",out]
  gzip out

makeMask :: FilePath -> FilePath -> Action()
makeMask invol outvol = do
      command_ [] "unu" ["3op","ifelse",invol,"1","0","-o",outvol]
      gzip outvol
