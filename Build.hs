import PNLPipeline
import Intrust

outdir = "_data"
mkpath name caseid = outdir </> name </> (caseid ++ "-" ++ name) <.> "nrrd"

newtype Fa = Fa CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode Fa where
    path (Fa caseid) = mkpath "fa" caseid
    nodeAction n@(Fa caseid) = do
        let dwiharm = Intrust.path "dwiharm" caseid
        need [dwiharm]
        Nrrd.fa dwiharm (path n)

newtype FsMaskCsf = FsMaskCsf CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FsMaskCsf where
    path (FsMaskCsf caseid) = mkpath "fsindwi-mask" caseid
    nodeAction n@(FsMaskCsf caseid) = do
        let fsindwi = Intrust.path "fsindwi" caseid
        need [fsindwi]
        Nrrd.makeMask fsindwi (path n)

newtype FsMask = FsMask CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FsMask where
    path (FsMask caseid) = mkpath "fsindwi-nocsf-mask" caseid
    nodeAction n@(FsMask caseid) = do
        let fsindwi = intrustPath "fsindwi" caseid
            script = "./src/famask.py" 
        need [fsindwi]
        command_ [] script [fsindwi, path n]

newtype FaMaskedCsf = FaMaskedCsf CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FaMaskedCsf where
    path (FaMaskedCsf x) = mkpath "fa-masked" x
    nodeAction node@(FaMaskedCsf x) = do
        apply1 $ Fa x :: Action Double
        apply1 $ FsMask x :: Action Double
        Nrrd.mask (path $ FsMask x) (path $ FA x) (path node)

newtype FaMasked = FaMasked CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
instance PNLNode FaMasked where
    path (FaMasked caseid) = mkpath "fa-masked-nocsf" caseid
    nodeAction node@(FaMasked caseid) = do
        let fa = Fa caseid
            mask = FsMask caseid
        apply1 fa :: Action Double
        apply1 mask :: Action Double
        nrrdMask (path mask) (path fa) (path node)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_data", shakeVerbosity=Chatty} $ do
    want ["_data/fastats.csv"
         ,"_data/fastats-nocsf.csv"]

    rule (nodeBuildAction :: FsMaskCsf-> Maybe (Action Double))
    rule (nodeBuildAction :: FsMask -> Maybe (Action Double))
    rule (nodeBuildAction :: Fa -> Maybe (Action Double))
    rule (nodeBuildAction :: FaMaskedCsf -> Maybe (Action Double))
    rule (nodeBuildAction :: FaMasked -> Maybe (Action Double))

    "_data/fastats-withcsf.csv" %> \out -> do
        Stdout caseids <- cmd "cases"
        let fas = [FaMaskedCsf caseid | caseid <- lines caseids]
            script = "src/fastats.j"
        apply fas  :: Action [Double]
        need [script]
        command_ [] script $ [out] ++ (map path fas)

    "_data/fastats.csv" %> \out -> do
        Stdout caseids <- cmd "cases"
        let fas = [FaMasked caseid | caseid <- lines caseids]
            script = "src/fastats.j"
        apply fas  :: Action [Double]
        need [script]
        command_ [] script $ [out] ++ (map path fas)
