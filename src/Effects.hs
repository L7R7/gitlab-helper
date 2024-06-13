{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects
  ( Writer (..),
    write,
    Timer (..),
    getCurrentTime,
    ProjectsApi (..),
    getProjects,
    getProject,
    MergeRequestApi (..),
    getOpenMergeRequests,
    enableSourceBranchDeletionAfterMrMerge,
    BranchesApi (..),
    getBranches,
    PipelinesApi (..),
    getPipeline,
    getSuccessfulPipelines,
    UpdateError (..),
    Branch (..),
    Project (..),
    MergeRequest (..),
    Duration (..),
    Sha (..),
    Pipeline (..),
    PipelineId (..),
    Ref (..),
    ProjectId (..),
    ProjectName (..),
    CompactPipeline (..),
  )
where

import Config
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, withObject, withText, (.:))
import Data.Aeson.Casing
import qualified Data.Text as T hiding (partition)
import Data.Time (UTCTime)
import Network.HTTP.Client.Conduit (HttpException)
import Network.HTTP.Simple (JSONException)
import Network.URI
import Polysemy
import Relude
import qualified Text.Show

newtype ProjectId = ProjectId Int deriving newtype (FromJSON, Show)

newtype ProjectName = ProjectName String deriving newtype (Eq, FromJSON, Show)

newtype Ref = Ref T.Text deriving newtype (FromJSON, Show)

data Project = Project
  { projectId :: ProjectId,
    name :: ProjectName,
    mergeRequestsEnabled :: Bool,
    defaultBranch :: Maybe Ref,
    removeSourceBranchAfterMerge :: Maybe Bool
  }

instance Show Project where
  show Project {..} =
    "----\n"
      <> show name
      <> " ("
      <> show projectId
      <> ")\nMRs enabled:"
      <> show mergeRequestsEnabled
      <> ",\tdefault branch: "
      <> show defaultBranch
      <> ",\tremove source branch after merge: "
      <> show removeSourceBranchAfterMerge

instance FromJSON Project where
  parseJSON = withObject "Project" $ \p ->
    Project <$> (p .: "id")
      <*> (p .: "name")
      <*> (p .: "merge_requests_enabled")
      <*> (p .: "default_branch")
      <*> (p .: "remove_source_branch_after_merge")

instance FromJSON URI where
  parseJSON = withText "URI" $ \v -> maybe (fail "Bad URI") pure (parseURI (toString v))

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
  { branchName :: T.Text,
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
  | ExceptionError SomeException
  | ConversionError JSONException
  deriving (Show)

data Writer m a where
  Write :: Text -> Writer m ()

makeSem ''Writer

data Timer m a where
  GetCurrentTime :: Timer m UTCTime

makeSem ''Timer

data ProjectsApi m a where
  GetProjects :: GroupId -> ProjectsApi m (Either UpdateError [Project])
  GetProject :: ProjectId -> ProjectsApi m (Either UpdateError Project)

makeSem ''ProjectsApi

data MergeRequestApi m a where
  GetOpenMergeRequests :: ProjectId -> MergeRequestApi m (Either UpdateError [MergeRequest])
  EnableSourceBranchDeletionAfterMrMerge :: ProjectId -> MergeRequestApi m (Either UpdateError ())

makeSem ''MergeRequestApi

data BranchesApi m a where
  GetBranches :: ProjectId -> BranchesApi m (Either UpdateError [Branch])

makeSem ''BranchesApi

newtype PipelineId = PipelineId Int deriving newtype (Eq, FromJSON, Num, Ord, Show)

newtype Duration = Duration Int deriving newtype (FromJSON, Show)

instance ToText Duration where
  toText (Duration duration) = show duration

newtype Sha = Sha T.Text deriving newtype (FromJSON, Show)

data Pipeline = Pipeline
  { pipelineId :: PipelineId,
    pipelineSha :: Sha,
    pipelineDuration :: Maybe Duration,
    pipelineCreatedAt :: UTCTime,
    pipelineWebUrl :: URI
  }
  deriving (Generic, Show)

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data CompactPipeline = CompactPipeline
  { compactPipelineId :: PipelineId,
    compactPipelineSha :: Sha
  }
  deriving (Generic)

instance FromJSON CompactPipeline where
  parseJSON = genericParseJSON $ aesonDrop 15 snakeCase

data PipelinesApi m a where
  GetPipeline :: ProjectId -> PipelineId -> PipelinesApi m (Either UpdateError Pipeline)
  GetSuccessfulPipelines :: ProjectId -> Ref -> PipelinesApi m (Either UpdateError [CompactPipeline])

makeSem ''PipelinesApi
