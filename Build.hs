{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
import           Data.List   (intercalate)
import qualified Intrust
import qualified Nrrd        (fa, makeMask, mask)
import           PNLPipeline

outdir = "_data"
{-keyToPath :: Show k => FilePath -> k -> String -> CaseId -> FilePath-}
{-keyToPath dir key ext x = outdir </> dir </> (x++"-"++show key) <.> ext-}

{-showKey :: Show k => k -> String-}
{-showKey key = intercalate "-" (words . show $ key)-}

keyToFile :: Show k => Bool -> k -> String -> String
keyToFile hasKey key ext  =
    let 
       keywords = map (filter (/='"')) (words . show $ key)
       caseid = last keywords
       key' = if True then intercalate "-" (caseid:init keywords)
              else intercalate "-" keywords
    in
       if null ext then key' else key' <.> ext


---------------------------------------------------------------
-- UKF

data UkfType = UkfCpp | UkfMatlab
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data UKF = UKF DwiType UkfType CaseId
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey UKF where
  path (UKF DwiHarm UkfCpp caseid) = Intrust.path "ukf_dwiharm_cpp" caseid
  path _ = error "No intrust result for this ukf type"


---------------------------------------------------------------
-- FiberLengths

data FiberLengths = FiberLengths DwiType UkfType CaseId
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey FiberLengths where
  path key = outdir </> "fiberlengths" </> keyToFile True key "txt"

  build out@(FiberLengths dwitype ukftype caseid) = case (dwitype, ukftype) of
    (DwiHarm, UkfCpp) -> Just $ do
        let tmpvtk = "/tmp/" ++ caseid ++ ".vtk"
            ukf = UKF dwitype ukftype caseid
        apply1 ukf :: Action Double
        withTempFile $ \tmpvtk -> do
          unit $ cmd Shell "gunzip -c" (path ukf) ">" tmpvtk
          need ["src/fiberlengths.j"]
          command_ [] "src/fiberlengths.j" [tmpvtk, path out]
    (DwiEd, UkfMatlab) -> Nothing
      

---------------------------------------------------------------
-- DWI
data DwiType = DwiEd | DwiHarm
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data Dwi = Dwi DwiType CaseId
             deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey Dwi where
  path (Dwi DwiEd x) = Intrust.path "dwied" x
  path (Dwi DwiHarm x) = Intrust.path "dwiharm" x

---------------------------------------------------------------
-- FA Mask
data FaMaskType = FsMask | FsMaskNoCsf
                deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

data FaMask = FaMask FaMaskType CaseId
                deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance BuildKey FaMask where
    path key = outdir </> "famask" </> keyToFile True key "nrrd"

    build out@(FaMask FsMask caseid) = Just $ do
        let fsindwi = Intrust.path "fsindwi" caseid
            dwi = Dwi DwiEd caseid
        need [fsindwi, "src/downsampleNN.sh"]
        apply1 dwi :: Action Double
        Nrrd.makeMask fsindwi (path out)
        command_ [] "src/downsampleNN.sh" ["-i", path out
                                          ,"-r", path dwi
                                          ,"-o", path out]
    build out@(FaMask FsMaskNoCsf caseid) = Just $ do
        let fsindwi = Intrust.path "fsindwi" caseid
            dwi = Dwi DwiEd caseid
            script = "./src/famask.py"
        apply1 dwi :: Action Double
        need [fsindwi, "src/downsampleNN.sh"]
        command_ [] script [fsindwi, path out]
        command_ [] "src/downsampleNN.sh" ["-i", path out
                                          ,"-r", path dwi
                                          ,"-o", path out]

---------------------------------------------------------------
-- FA
data FA = FA DwiType (Maybe FaMaskType) CaseId
        deriving (Generic,Typeable,Eq,Hashable,Binary,NFData)

instance Show FA where
  show (FA dwitype Nothing caseid) = intercalate "-" [caseid, "FA", show dwitype]
  show (FA dwitype (Just m) caseid) = intercalate "-" [caseid, "FA", show dwitype, show m]

instance BuildKey FA where
  path key = outdir </> "fa" </> show key <.> "nrrd"

  build this@(FA dwi Nothing caseid) = Just $ do
    apply1 (Dwi dwi caseid) :: Action Double
    Nrrd.fa (path (Dwi dwi caseid)) (path this)

  build this@(FA dwi (Just masktype) caseid) = Just $ do
    let fa = FA dwi Nothing caseid
        famask = FaMask masktype caseid
    apply1 fa :: Action Double
    apply1 famask :: Action Double
    Nrrd.mask (path famask) (path fa) (path this)


---------------------------------------------------------------
-- FA Stats
data FaStats = FaStats DwiType FaMaskType
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

{-instance Show FaStats where-}
    {-show (FaStats d m) = intercalate "-" ["FaStats", show d, show m]-}

instance BuildKey FaStats where
  path k = outdir </> "fastats" </> keyToFile False k "csv"

  build this@(FaStats dwi mask) = Just $ do
    Stdout caselist <- cmd "cases"
    let fas = [FA dwi (Just mask) x | x <- lines caselist]
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
        let fastats = [FaStats d m
                   | d <- [DwiHarm, DwiEd]
                   , m <- [FsMask, FsMaskNoCsf]
                   ]
        apply fastats :: Action [Double]
        {-Stdout caseids <- cmd "cases"-}
        {-let fiberlengths = [FiberLengths DwiEd UkfMatlab caseid | caseid <- take 5 . lines $ caseids]-}
        {-apply fiberlengths :: Action [Double]-}

    ["_data/fiber_counts.csv",
     "_data/fiber_lengths_sample.csv"] &%> \[countsCsv, lengthsCsv] -> do
      Stdout caselist <- cmd "cases"
      let matlabFiberLengths = [FiberLengths DwiEd UkfMatlab caseid | caseid <- lines caselist ]
          cppFiberLengths = [FiberLengths DwiHarm UkfCpp caseid | caseid <- lines caselist ]
          fiberLengths = matlabFiberLengths ++ cppFiberLengths
      apply $ fiberLengths :: Action [Double]
      need ["src/summarize-fiberlengths.py"]
      unit $ cmd "src/summarize-fiberlengths.py" (map path fiberLengths)

    rule (buildKey :: Dwi -> Maybe (Action Double))
    rule (buildKey :: FaMask -> Maybe (Action Double))
    rule (buildKey :: FA -> Maybe (Action Double))
    rule (buildKey :: FaStats -> Maybe (Action Double))
    rule (buildKey :: UKF -> Maybe (Action Double))
    rule (buildKey :: FiberLengths -> Maybe (Action Double))


