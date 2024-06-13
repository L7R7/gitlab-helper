{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.File (readPartialOptionsFromHomeDir, readPartialOptionsFromLocalDir) where

import Config.Types
import Data.Aeson
import Data.Yaml
import Network.URI (parseURI)
import Relude
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)

instance FromJSON PartialConfig where
  parseJSON = withObject "PartialConfig" $ \o ->
    o .: "config" >>= \c -> do
      pGroupId <- c .:? "groupId"
      pBaseUrl <- c .:? "baseUrl"
      pApiToken <- c .:? "apiToken"
      pure $ PartialConfig {..}

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
  exists <- doesFileExist configFilePath
  if exists
    then fromRight mempty <$> decodeFileEither configFilePath
    else pure mempty
