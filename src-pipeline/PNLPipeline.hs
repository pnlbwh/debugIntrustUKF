{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds     #-}
module PNLPipeline
  (module Development.Shake
  ,module Development.Shake.Command
  ,module Development.Shake.FilePath
  ,module Development.Shake.Classes
  ,module Development.Shake.Rule
  ,module GHC.Generics
  ,PNLNode (..)
  ,nodeBuildAction
  ,CaseId
  )
  where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Development.Shake.Rule (rule,apply1,apply, Rule(..), EqualCost(..))
import Development.Shake.Classes
import Text.Printf
import           GHC.Generics
import Data.Time (UTCTime (..), utctDayTime)
import System.Directory as IO


-- Shake rules
--

getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime
type CaseId = String
type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

class PNLNode a where
    path :: a -> FilePath
    nodeAction :: a -> Action ()

instance (ShakeKey k, PNLNode k) => Rule k Double where
    storedValue _ q = do
        exists <- IO.doesFileExist $ path q
        if not exists then return Nothing
          else fmap Just $ getModTime $ path q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

nodeBuildAction :: (PNLNode a) => a -> Maybe (Action Double)
nodeBuildAction k = Just $ do
      liftIO . createDirectoryIfMissing True . takeDirectory . path $ k
      nodeAction k
      liftIO $ getModTime . path $ k

-- PNL types
--

-- newtype WmparcInDwi = WmparcInDwi CaseId deriving (Generic,Typeable,Show,Eq,Hashable,Binary,NFData)
-- instance PNLNode WmparcInDwi where
--   path env (WmparcInDwi caseid) = 
--   nodeAction n@(WmparcInDwi caseid) = do
--     let script = "src/WmparcInDwi/fs2dwi_T2.sh"
--     need[script]
--     command_ [] script []
