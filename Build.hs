{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
import PNLPipeline
import qualified Intrust
import qualified Nrrd (mask, fa, makeMask)
import Data.List (intercalate)

outdir = "_data"
mkpath name caseid = outdir </> name </> (caseid ++ "-" ++ name) <.> "nrrd"

data FsMask = FsMask CaseId 
            | FsMaskNoCsf CaseId 
            deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance PNLNode FsMask where
    path (FsMask caseid) = mkpath "FsMask" caseid
    path (FsMaskNoCsf caseid) = mkpath "MaskNoCsf" caseid
    nodeAction n@(FsMask caseid) = do
        let fsindwi = Intrust.path "fsindwi" caseid
        need [fsindwi]
        Nrrd.makeMask fsindwi (path n)
    nodeAction n@(FsMaskNoCsf caseid) = do
        let fsindwi = Intrust.path "fsindwi" caseid
            script = "./src/famask.py" 
        need [fsindwi]
        command_ [] script [fsindwi, path n]

data DwiType = DwiEd CaseId
             | DwiHarm CaseId
            deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

instance PNLNode DwiType where
    path (DwiEd x) = Intrust.path "dwied" x
    path (DwiHarm x) = Intrust.path "dwiharm" x
    nodeAction n = need [path n]

data FA = FA DwiType (Maybe FsMask)
    deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)

showtype :: Show a => a -> String
showtype = head . words . show
showid :: Show a => a -> String
showid = tail . init . last . words . show

instance PNLNode FA where
    path (FA dwikey Nothing) = outdir </> "fa" </> (intercalate "-" [showid dwikey, showtype dwikey, "fa"]) <.> "nrrd"
    path (FA dwikey (Just maskkey)) = outdir </> "fa" </> (intercalate "-" [showid dwikey, showtype dwikey,"fa",showtype maskkey]) <.> "nrrd"

    nodeAction this@(FA dwikey Nothing) = do 
        apply1 dwikey :: Action Double
        Nrrd.fa (path dwikey) (path this)

    nodeAction this@(FA dwikey (Just mask)) = do
        let fa_src = FA dwikey Nothing
        apply1 fa_src :: Action Double
        apply1 mask :: Action Double
        Nrrd.mask (path mask) (path fa_src) (path this)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_data", shakeVerbosity=Chatty} $ do
    {-want ["_data/fastats-csf.csv"-}
         {-,"_data/fastats.csv"-}
         {-,"_data/fastats-orig.csv"]-}
    action $ do
        let key = "003_GNX_007"
            keys = [FA (d key) (Just (m key)) | d <- [DwiHarm,DwiEd], m <- [FsMask,FsMaskNoCsf]]
        apply keys :: Action [Double]

    rule (nodeBuildAction :: DwiType -> Maybe (Action Double))
    rule (nodeBuildAction :: FsMask -> Maybe (Action Double))
    rule (nodeBuildAction :: FA -> Maybe (Action Double))

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


