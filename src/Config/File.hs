{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config.File (readPartialOptionsFromHomeDir, readPartialOptionsFromLocalDir) where

import Autodocodec
import Barbies (bmap)
import Config.Types
import Data.Aeson
import qualified Data.Semigroup as S
import Data.Yaml
import Gitlab.Client.MTL (BaseUrl (..))
import Gitlab.Lib (Id)
import Network.URI (parseURI)
import Relude
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)

instance FromJSON (PartialConfig Maybe) where
  parseJSON = withObject "PartialConfig" $ \o ->
    o .: "config" >>= \c -> do
      pGroupId <- c .:? "groupId"
      pBaseUrl <- c .:? "baseUrl"
      pApiToken <- c .:? "apiToken"
      pUserAgent <- c .:? "userAgent"
      pProjectsExcludeList <- c .:? "excludeProjects"
      let pCommand = Nothing
      pure $ PartialConfig {..}

deriving via (Autodocodec (Id a)) instance FromJSON (Id a)

instance FromJSON (PartialConfig (Compose Maybe S.First)) where
  parseJSON = fmap (bmap (Compose . fmap S.First)) . (parseJSON @(PartialConfig Maybe))

instance FromJSON BaseUrl where
  parseJSON = withText "URI" $ \v ->
    maybe
      (fail "Bad URI")
      (pure . BaseUrl)
      (parseURI (toString v))

readPartialOptionsFromHomeDir :: (Monoid a, FromJSON a) => IO a
readPartialOptionsFromHomeDir = getHomeDirectory >>= readPartialOptionsFromDir

readPartialOptionsFromLocalDir :: (Monoid a, FromJSON a) => IO a
readPartialOptionsFromLocalDir = getCurrentDirectory >>= readPartialOptionsFromDir

readPartialOptionsFromDir :: (Monoid a, FromJSON a) => FilePath -> IO a
readPartialOptionsFromDir dir = do
  let configFilePath = dir <> "/.gitlab-helper.yml"
  ifM
    (doesFileExist configFilePath)
    (fromRight mempty <$> decodeFileEither configFilePath)
    (pure mempty)
