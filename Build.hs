{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Data.List   (intercalate)
import qualified Intrust
import qualified Nrrd        (fa, makeMask, mask)
import           PNLPipeline

outdir = "_data"
keyToPath :: Show k => FilePath -> k -> String -> CaseId -> FilePath
keyToPath dir key ext x = outdir </> dir </> (x++"-"++show key) <.> ext

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

    build n@(FsMask, caseid) = Just $ do
        let fsindwi = Intrust.path "fsindwi" caseid
        need [fsindwi]
        Nrrd.makeMask fsindwi (path n)
    build n@(FsMaskNoCsf, caseid) = Just $ do
        let fsindwi = Intrust.path "fsindwi" caseid
            script = "./src/famask.py"
        need [fsindwi]
        command_ [] script [fsindwi, path n]

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

instance FaStats Show where
    show (FaStats d m) = intercalate "-" ["FaStats",d,m]

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
    {-want ["_data/fastats-csf.csv"-}
         {-,"_data/fastats.csv"-}
         {-,"_data/fastats-orig.csv"]-}
    action $ do
        let x = "003_GNX_007"
            keys = [FaStats d m
                   | d <- [DwiHarm, DwiEd]
                   , m <- [FsMask, FsMaskNoCsf]
                   ]
        apply keys :: Action [Double]

    rule (buildKey :: (DwiType, CaseId) -> Maybe (Action Double))
    rule (buildKey :: (FsMask, CaseId) -> Maybe (Action Double))
    rule (buildKey :: (FA, CaseId) -> Maybe (Action Double))
    rule (buildKey :: FaStats -> Maybe (Action Double))

    {-"_data/fastats.csv" %> \out -> do-}
        {-Stdout caseids <- cmd "cases"-}
        {-let fas = [FaMaskedCsf caseid | caseid <- lines caseids]-}
            {-script = "src/fastats.j"-}
        {-apply fas  :: Action [Double]-}
        {-need [script]-}
        {-command_ [] script $ [out] ++ (map path fas)-}

    {-"_data/fastats-csf.csv" %> \out -> do-}
        {-Stdout caseids <- cmd "cases"-}
        {-let fas = [FaMasked caseid | caseid <- lines caseids]-}
            {-script = "src/fastats.j"-}
        {-apply fas  :: Action [Double]-}
        {-need [script]-}
        {-command_ [] script $ [out] ++ (map path fas)-}

    {-"_data/fastats-orig.csv" %> \out -> do-}
        {-Stdout caseids <- cmd "cases"-}
        {-let fas = [FaMaskedO caseid | caseid <- lines caseids]-}
            {-script = "src/fastats.j"-}
        {-apply fas  :: Action [Double]-}
        {-need [script]-}
        {-command_ [] script $ [out] ++ (map path fas)-}
