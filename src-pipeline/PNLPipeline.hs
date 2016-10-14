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
  ,CaseId
  ,BuildKey (..)
  ,buildKey
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


getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime

type CaseId = String
type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

class BuildKey a where
  path :: a -> FilePath
  build :: a -> Maybe (Action ())
  build _ = Nothing

instance (ShakeKey k, BuildKey k) => Rule k Double where
    storedValue _ q = do
        exists <- IO.doesFileExist $ path q
        if not exists then return Nothing
          else fmap Just $ getModTime $ path q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildKey :: BuildKey a => a -> Maybe (Action Double)
buildKey k = case (build k) of
  Nothing -> Just $ liftIO $ getModTime . path $ k
  (Just action) -> Just $ do
      liftIO . createDirectoryIfMissing True . takeDirectory . path $ k
      action
      liftIO $ getModTime . path $ k