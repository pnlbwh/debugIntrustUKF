{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds     #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Development.Shake.Rule
import Development.Shake.Classes
import Text.Printf
import           GHC.Generics
import Data.Time (UTCTime (..), utctDayTime)
import System.Directory as IO

getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime
type CaseId = String
type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

class PNLNode a where
    path :: a -> FilePath
    execute :: a -> Action ()

instance (ShakeKey k, PNLNode k) => Rule k Double where
    storedValue _ q = do
        exists <- IO.doesFileExist $ path q
        if not exists then return Nothing
          else fmap Just $ getModTime $ path q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

nodeAction :: (PNLNode a) => a -> Maybe (Action Double) 
nodeAction k = Just $ do
      liftIO . createDirectoryIfMissing True . takeDirectory . path $ k
      execute k
      liftIO $ getModTime . path $ k

intrustPath :: String -> CaseId -> FilePath
intrustPath var siteid =
  case var of
    "fsindwi" -> printf (base </> "%s/diff/%s.fsindwi.nrrd") siteid siteid
    "dwiharm" -> printf (base </> "Harmonization-20160728/%s_hmz_iter1.nhdr") siteid
    "caselist" -> (base </> "caselist.txt")
  where base = "/data/pnl/INTRuST/"
  {-where base = "/Users/ryan/partners/data/pnl/INTRuST/"-}

outdir = "_data"
pre name caseid = outdir </> name </> (caseid ++ "-" ++ name) <.> "nrrd"
nrrdZip out = command_ [] "unu" ["save","-e","gzip","-f","nrrd","-i",out,"-o",out]
nrrdMask :: FilePath -> FilePath -> FilePath -> Action ()
nrrdMask mask vol out = do 
    unit $ cmd "unu 3op ifelse" mask vol "0" "-o" out
    nrrdZip out

newtype FA = FA CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FA where
    path (FA caseid) = pre "fa" caseid
    execute n@(FA caseid) = do
        let dwiharm = intrustPath "dwiharm" caseid
        need [dwiharm]
        withTempFile $ \tensor -> do
          command_ [] "tend" ["estim","-est","lls","-B","kvp","-knownB0","true","-i",dwiharm,"-o",tensor]
          command_ [] "tend" ["anvol","-t","-1","-a","fa","-i",tensor,"-o",path n]
        nrrdZip $ path n

newtype FSinDWIMask = FSinDWIMask CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FSinDWIMask where
    path (FSinDWIMask caseid) = pre "fsindwi-mask" caseid
    execute n@(FSinDWIMask caseid) = do
        let fsindwi = intrustPath "fsindwi" caseid
        need [fsindwi]
        command_ [] "unu" ["3op","ifelse",fsindwi,"1","0","-o",path n]
        nrrdZip $ path n

newtype FSinDWIMaskNoCSF = FSinDWIMaskNoCSF CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FSinDWIMaskNoCSF where
    path (FSinDWIMaskNoCSF caseid) = pre "fsindwi-nocsf-mask" caseid
    execute n@(FSinDWIMaskNoCSF caseid) = do
        let fsindwi = intrustPath "fsindwi" caseid
        need [fsindwi]
        command_ [] "./src/famask.py" [fsindwi, path n]

newtype FAmasked = FAmasked CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FAmasked where
    path (FAmasked caseid) = pre "fa-masked" caseid
    execute n@(FAmasked caseid) = do
        apply [FA caseid] :: Action [Double]
        apply [FSinDWIMask caseid] :: Action [Double]
        nrrdMask (path $ FSinDWIMask caseid) (path $ FA caseid) (path n)

newtype FAmaskedNoCSF = FAmaskedNoCSF CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FAmaskedNoCSF where
    path (FAmaskedNoCSF caseid) = pre "fa-masked-nocsf" caseid
    execute n@(FAmaskedNoCSF caseid) = do
        let fa = FA caseid
            mask = FSinDWIMaskNoCSF caseid
        apply [fa] :: Action [Double]
        apply [mask] :: Action [Double]
        nrrdMask (path mask) (path fa) (path n)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_data", shakeVerbosity=Chatty} $ do
    {-action $ do-}
        {-Stdout caseids <- cmd "cases"-}
        {-apply [FAmasked caseid | caseid <- lines caseids] :: Action [Double]-}
        {-apply [FAmaskedNoCSF caseid | caseid <- lines caseids] :: Action [Double]-}

    want ["_data/fastats.csv"
         ,"_data/fastats-nocsf.csv"]

    rule (nodeAction :: FSinDWIMask -> Maybe (Action Double))
    rule (nodeAction :: FSinDWIMaskNoCSF -> Maybe (Action Double))
    rule (nodeAction :: FA -> Maybe (Action Double))
    rule (nodeAction :: FAmasked -> Maybe (Action Double))
    rule (nodeAction :: FAmaskedNoCSF -> Maybe (Action Double))

    "_data/fastats.csv" %> \out -> do
        Stdout caseids <- cmd "cases"
        let fas = [FAmasked caseid | caseid <- lines caseids]
        apply fas  :: Action [Double]
        command_ [] "src/fastats.j" $ [out] ++ (map path fas)

    "_data/fastats-nocsf.csv" %> \out -> do
        Stdout caseids <- cmd "cases"
        let fas = [FAmaskedNoCSF caseid | caseid <- lines caseids]
        apply fas  :: Action [Double]
        command_ [] "src/fastats.j" $ [out] ++ (map path fas)
