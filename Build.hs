{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
import           Data.List   (intercalate)
import qualified Intrust
import qualified Nrrd        (fa, makeMask, mask)
import           PNLPipeline

outdir = "_data"
keyToPath :: Show k => FilePath -> k -> String -> CaseId -> FilePath
keyToPath dir key ext x = outdir </> dir </> (x++"-"++show key) <.> ext

showKey :: Show k => k -> String
showKey key = intercalate "-" (words . show $ key)

---------------------------------------------------------------
-- UKFType

data UkfType = UkfCpp | UkfMatlab
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

---------------------------------------------------------------
-- UKF

data UKF = UKF DwiType UkfType
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey (UKF, CaseId) where
  path (UKF DwiHarm UkfCpp, caseid) = Intrust.path "ukf_dwiharm_cpp" caseid
  path _ = error "No intrust result for this ukf type"


---------------------------------------------------------------
-- FiberLengths

newtype FiberLengths = FiberLengths UKF
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey (FiberLengths, CaseId) where
  path (FiberLengths ukf@(UKF DwiHarm UkfCpp), caseid) = outdir </> "fiberlengths"
    </> caseid ++ "-" ++ "FiberLengths" ++ "-" ++ showKey ukf ++ ".txt"

  build out@(FiberLengths ukf@(UKF DwiHarm UkfCpp), caseid) = Just $ do
    let tmpvtk = "/tmp/" ++ caseid ++ ".vtk"
        vtkgz = path (ukf, caseid)
    apply1 (ukf, caseid) :: Action Double
    withTempFile $ \tmpvtk -> do
      command_ [] "gunzip" ["-c", vtkgz, tmpvtk]
      need ["src/fiberlengths.j"]
      command_ [] "src/fiberlengths.j" [tmpvtk, path out]

---------------------------------------------------------------
-- DWI
data DwiType = DwiEd | DwiHarm
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey (DwiType, CaseId) where
  path (DwiEd, x) = Intrust.path "dwied" x
  path (DwiHarm, x) = Intrust.path "dwiharm" x

---------------------------------------------------------------
-- FA Mask
data FsMask = FsMask
            | FsMaskNoCsf
            deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey (FsMask, CaseId) where
    path (k, x) = keyToPath "famask" k "nrrd" x

    build out@(FsMask, caseid) = Just $ do
        let fsindwi = Intrust.path "fsindwi" caseid
            dwi = (DwiEd, caseid)
        need [fsindwi, "src/downsampleNN.sh"]
        apply1 dwi :: Action Double
        Nrrd.makeMask fsindwi (path out)
        command_ [] "src/downsampleNN.sh" ["-i", path out
                                          ,"-r", path dwi
                                          ,"-o", path out]
    build out@(FsMaskNoCsf, caseid) = Just $ do
        let fsindwi = Intrust.path "fsindwi" caseid
            dwi = (DwiEd, caseid)
            script = "./src/famask.py"
        apply1 dwi :: Action Double
        need [fsindwi, "src/downsampleNN.sh"]
        command_ [] script [fsindwi, path out]
        command_ [] "src/downsampleNN.sh" ["-i", path out
                                          ,"-r", path dwi
                                          ,"-o", path out]

---------------------------------------------------------------
-- FA
data FA = FA DwiType (Maybe FsMask)
        deriving (Generic,Typeable,Eq,Hashable,Binary,NFData)
instance Show FA where
  show (FA dwitype Nothing) = "FA-" ++ (show dwitype)
  show (FA dwitype (Just x)) = intercalate "-" ["FA", show dwitype, show x]

instance BuildKey (FA, CaseId) where
  path (key, x) = keyToPath "fa" key "nrrd" x

  build this@((FA dwi Nothing), x) = Just $ do
    apply1 (dwi, x) :: Action Double
    Nrrd.fa (path (dwi, x)) (path this)

  build this@((FA dwi (Just mask)), x) = Just $ do
    let faSrc = FA dwi Nothing
    apply1 (faSrc, x) :: Action Double
    apply1 (mask, x) :: Action Double
    Nrrd.mask (path (mask,x)) (path (faSrc,x)) (path this)


---------------------------------------------------------------
-- FA Stats
data FaStats = FaStats DwiType FsMask
        deriving (Generic,Typeable,Eq,Hashable,Binary,NFData)

instance Show FaStats where
    show (FaStats d m) = intercalate "-" ["FaStats", show d, show m]

instance BuildKey FaStats where
  path k = outdir </> "fastats" </> (show k) <.> "csv"

  build this@(FaStats dwi mask) = Just $ do
    Stdout caselist <- cmd "cases"
    let fas = [(FA dwi (Just mask), x) | x <- lines caselist]
    apply fas :: Action [Double]
    need ["src/fastats.j"]
    command_ [] "src/fastats.j" $ [path this] ++ (map path fas)


---------------------------------------------------------------
-- main
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
    want ["_data/fiber_lengths_sample.csv",
          "_data/fiber_counts.csv"]
    action $ do
        let keys = [FaStats d m
                   | d <- [DwiHarm, DwiEd]
                   , m <- [FsMask, FsMaskNoCsf]
                   ]
        apply keys :: Action [Double]

    rule (buildKey :: (DwiType, CaseId) -> Maybe (Action Double))
    rule (buildKey :: (FsMask, CaseId) -> Maybe (Action Double))
    rule (buildKey :: (FA, CaseId) -> Maybe (Action Double))
    rule (buildKey :: FaStats -> Maybe (Action Double))

    ["_data/fiber_counts.csv",
     "_data/fiber_lengths_sample.csv"] &%> \[countsCsv, lengthsCsv] -> do
      need ["src/mkcsvs.py"]
      getDirectoryFiles "_data/tractlengths" ["*.txt"]
      unit $ cmd "src/mkcsvs.py"
