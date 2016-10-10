{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
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

data OutputType
  = FSinDWIMask
  | FSinDWIMaskNoCSF
  deriving (Eq,Show,Generic)

instance Binary OutputType
instance Hashable OutputType
instance NFData OutputType

type CaseId = String

newtype OutputKey = OutputKey (OutputType, CaseId)
    deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

outdir = "_data"
pre name caseid = outdir </> name </> (caseid ++ "-" ++ name) <.> "nrrd"

getFile :: OutputKey -> FilePath
getFile (OutputKey (outputtype, caseid)) = case outputtype of
  FSinDWIMask -> pre "fsindwi-mask" caseid
  FSinDWIMaskNoCSF -> pre "fsindwi-nocsf-mask" caseid
  _ -> error "Invalid output key"

instance Rule OutputKey Double where
    storedValue _ q = do
        exists <- IO.doesFileExist $ getFile q
        if not exists then return Nothing
          else fmap Just $ getModTime $ getFile q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

getPath :: String -> CaseId -> FilePath
getPath var siteid =
  {-let base = "/Users/ryan/partners/data/pnl/INTRuST/"-}
  let base = "/data/pnl/INTRuST/"
  in case var of
    "fsindwi" -> printf (base </> "%s/diff/%s.fsindwi.nrrd") siteid siteid
    "caselist" -> (base </> "caselist.txt")
    _ -> error "Invalid path filepattern variable"

makeTarget q@(OutputKey (outputtype, caseid)) =
  case outputtype of
      FSinDWIMask -> do
        need [fsindwi]
        command_ [] "unu" ["3op","ifelse",fsindwi,"1","0","-o",out]
        command_ [] "unu" ["save","-e","gzip","-f","nrrd","-i",out,"-o",out]
      FSinDWIMaskNoCSF -> do
        need [fsindwi]
        cmd Shell $ "\
        \export PYTHONPATH=;\
        \source /data/pnl/soft/anaconda3/bin/activate py27;\
        \./src/famask.py " ++ unwords [fsindwi, out]
    where
      out = getFile q
      fsindwi = getPath "fsindwi" caseid

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_data", shakeVerbosity=Chatty} $ do
    {-action $ do-}
     {-cases <- getCases   -}
     {-want [fsindwi-mask | fsindwimask <- getVar -}
    action $ (apply [OutputKey (FSinDWIMask, "003_GNX_007")] :: Action [Double])
    action $ (apply [OutputKey (FSinDWIMaskNoCSF, "003_GNX_007")] :: Action [Double])

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "data" ["//*"]

    rule $ \q -> Just $ do
      liftIO . createDirectoryIfMissing True . takeDirectory . getFile $ q
      makeTarget q
      liftIO $ getModTime . getFile $ q
