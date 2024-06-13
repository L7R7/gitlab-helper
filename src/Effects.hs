{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects where

import Config
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, withText, (.:))
import qualified Data.Text as TX hiding (partition)
import Data.Time (UTCTime)
import Network.HTTP.Client.Conduit (HttpException)
import Network.HTTP.Simple (JSONException)
import Network.URI
import Polysemy

newtype ProjectId = ProjectId Int deriving newtype (Show, ToJSON)

newtype ProjectName = ProjectName String deriving newtype (Show)

data Project = Project
  { projectId :: ProjectId,
    name :: ProjectName,
    mergeRequestsEnabled :: Bool
  }
  deriving (Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \p ->
    Project <$> (ProjectId <$> p .: "id")
      <*> (ProjectName <$> p .: "name")
      <*> (p .: "merge_requests_enabled")

instance FromJSON URI where
  parseJSON = withText "URI" $ \v -> maybe (fail "Bad URI") pure (parseURI (TX.unpack v))

data MergeRequest = MergeRequest
  { mergeRequestId :: MergeRequestId,
    wip :: Bool,
    conflicts :: Bool,
    createdAt :: UTCTime,
    webUrl :: URI
  }
  deriving (Show)

instance FromJSON MergeRequest where
  parseJSON = withObject "MergeRequest" $ \p ->
    MergeRequest <$> (MergeRequestId <$> p .: "id")
      <*> p .: "work_in_progress"
      <*> p .: "has_conflicts"
      <*> p .: "created_at"
      <*> p .: "web_url"

newtype MergeRequestId = MergeRequestId Int deriving newtype (Show, ToJSON)

data Branch = Branch
  { branchName :: TX.Text,
    branchMerged :: Bool,
    branchProtected :: Bool,
    branchDefault :: Bool,
    branchWebUrl :: URI,
    branchCommittedDate :: UTCTime
  }
  deriving (Show)

instance FromJSON Branch where
  parseJSON = withObject "Branch" $ \p ->
    Branch <$> p .: "name"
      <*> p .: "merged"
      <*> p .: "protected"
      <*> p .: "default"
      <*> p .: "web_url"
      <*> (p .: "commit" >>= \c -> c .: "committed_date")

data UpdateError
  = HttpError HttpException
  | ConversionError JSONException
  deriving (Show)

data Writer m a where
  Write :: String -> Writer m ()

makeSem ''Writer

data Timer m a where
  GetCurrentTime :: Timer m UTCTime

makeSem ''Timer

data ProjectsApi m a where
  GetProjects :: GroupId -> ProjectsApi m (Either UpdateError [Project])

makeSem ''ProjectsApi

data MergeRequestApi m a where
  GetOpenMergeRequests :: ProjectId -> MergeRequestApi m (Either UpdateError [MergeRequest])

makeSem ''MergeRequestApi

data BranchesApi m a where
  GetBranches :: ProjectId -> BranchesApi m (Either UpdateError [Branch])

makeSem ''BranchesApi
