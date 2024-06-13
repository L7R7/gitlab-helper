{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects
  ( Writer (..),
    write,
    Timer (..),
    getCurrentTime,
    ProjectsApi (..),
    getProjects,
    getProject,
    hasCi,
    MergeRequestApi (..),
    getOpenMergeRequests,
    enableSourceBranchDeletionAfterMrMerge,
    setSuccessfulPipelineRequirementForMerge,
    unsetSuccessfulPipelineRequirementForMerge,
    setResolvedDiscussionsRequirementForMerge,
    BranchesApi (..),
    getBranches,
    PipelinesApi (..),
    getPipeline,
    getSuccessfulPipelines,
    SchedulesApi (..),
    getSchedules,
    UpdateError (..),
    Branch (..),
    Project (..),
    MergeRequest (..),
    Duration (..),
    Sha (..),
    Source (..),
    Pipeline (..),
    PipelineId (..),
    Ref (..),
    ProjectId (..),
    ProjectName (..),
    CompactPipeline (..),
    Schedule (..),
    EnabledDisabled (..),
  )
where

import Config.Types
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

newtype ProjectName = ProjectName {getProjectName :: Text} deriving newtype (Eq, FromJSON, Ord)

instance Show ProjectName where
  show = toString . getProjectName

newtype Ref = Ref T.Text deriving newtype (FromJSON, Show)

data Project = Project
  { projectId :: ProjectId,
    name :: ProjectName,
    mergeRequestsEnabled :: Bool,
    mergeMethod :: MergeMethod,
    defaultBranch :: Maybe Ref,
    removeSourceBranchAfterMerge :: Maybe Bool,
    onlyAllowMergeIfPipelineSucceeds :: Maybe Bool,
    onlyAllowMergeIfAllDiscussionsAreResolved :: Maybe Bool,
    autoCancelPendingPipelines :: EnabledDisabled
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
      <> ",\tonly allow merge if pipeline succeeds: "
      <> show onlyAllowMergeIfPipelineSucceeds
      <> ",\tonly allow merge if all discussions are resolved: "
      <> show onlyAllowMergeIfAllDiscussionsAreResolved
      <> ",\tauto cancel pending pipelines: "
      <> show autoCancelPendingPipelines

instance FromJSON Project where
  parseJSON = withObject "Project" $ \p ->
    Project <$> (p .: "id")
      <*> (p .: "name")
      <*> (p .: "merge_requests_enabled")
      <*> (p .: "merge_method")
      <*> (p .: "default_branch")
      <*> (p .: "remove_source_branch_after_merge")
      <*> (p .: "only_allow_merge_if_pipeline_succeeds")
      <*> (p .: "only_allow_merge_if_all_discussions_are_resolved")
      <*> (p .: "auto_cancel_pending_pipelines")

instance FromJSON URI where
  parseJSON = withText "URI" $ \v -> maybe (fail "Bad URI") pure (parseURI (toString v))

data EnabledDisabled = Enabled | Disabled
  deriving stock (Eq, Show)

instance FromJSON EnabledDisabled where
  parseJSON = withText "EnabledDisabled" $ \case
    "enabled" -> pure Enabled
    "disabled" -> pure Disabled
    _ -> fail "bad value"

data MergeMethod = Merge | RebaseMerge | FastForward
  deriving stock (Show)

instance FromJSON MergeMethod where
  parseJSON = withText "MergeMethod" $ \case
    "merge" -> pure Merge
    "rebase_merge" -> pure RebaseMerge
    "ff" -> pure FastForward
    _ -> fail "bad value"

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

data Schedule = Schedule
  { scheduleId :: Int,
    scheduleDescription :: Text,
    scheduleCron :: Text,
    scheduleCronTimezone :: Text,
    scheduleNextRunAt :: UTCTime,
    scheduleActive :: Bool,
    scheduleOwner :: Text
  }

instance FromJSON Schedule where
  parseJSON = withObject "Schedule" $ \s ->
    Schedule <$> s .: "id"
      <*> s .: "description"
      <*> s .: "cron"
      <*> s .: "cron_timezone"
      <*> s .: "next_run_at"
      <*> s .: "active"
      <*> (s .: "owner" >>= \o -> o .: "name")

data UpdateError
  = HttpError HttpException
  | ExceptionError SomeException
  | ConversionError JSONException
  | ParseUrlError Text
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
  HasCi :: ProjectId -> Ref -> ProjectsApi m (Either UpdateError Bool)

makeSem ''ProjectsApi

data MergeRequestApi m a where
  GetOpenMergeRequests :: ProjectId -> MergeRequestApi m (Either UpdateError [MergeRequest])
  EnableSourceBranchDeletionAfterMrMerge :: ProjectId -> MergeRequestApi m (Either UpdateError ())
  SetSuccessfulPipelineRequirementForMerge :: ProjectId -> MergeRequestApi m (Either UpdateError ())
  UnsetSuccessfulPipelineRequirementForMerge :: ProjectId -> MergeRequestApi m (Either UpdateError ())
  SetResolvedDiscussionsRequirementForMerge :: ProjectId -> MergeRequestApi m (Either UpdateError ())

makeSem ''MergeRequestApi

data BranchesApi m a where
  GetBranches :: ProjectId -> BranchesApi m (Either UpdateError [Branch])

makeSem ''BranchesApi

newtype PipelineId = PipelineId Int deriving newtype (Eq, FromJSON, Num, Ord, Show)

newtype Duration = Duration Double deriving newtype (FromJSON, Show, ToJSON)

instance ToText Duration where
  toText (Duration duration) = show duration

newtype Sha = Sha T.Text deriving newtype (FromJSON, Show)

data Pipeline = Pipeline
  { pipelineId :: PipelineId,
    pipelineSha :: Sha,
    pipelineDuration :: Maybe Duration,
    pipelineQueuedDuration :: Maybe Duration,
    pipelineCreatedAt :: UTCTime,
    pipelineWebUrl :: URI,
    pipelineSource :: Source
  }
  deriving (Generic, Show)

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Source = SourcePush | SourceWeb | SourceTrigger | SourceSchedule | SourceApi | SourceExternal | SourcePipeline | SourceChat | SourceWebide | SourceMergeRequestEvent | SourceExternalPullRequestEvent | SourceParentPipeline | SourceOndemandDastScan | SourceOndemandDastValidation deriving stock (Bounded, Enum, Eq, Generic, Show)

sourceToApiRep :: Source -> Text
sourceToApiRep SourcePush = "push"
sourceToApiRep SourceWeb = "web"
sourceToApiRep SourceTrigger = "trigger"
sourceToApiRep SourceSchedule = "schedule"
sourceToApiRep SourceApi = "api"
sourceToApiRep SourceExternal = "external"
sourceToApiRep SourcePipeline = "pipeline"
sourceToApiRep SourceChat = "chat"
sourceToApiRep SourceWebide = "webide"
sourceToApiRep SourceMergeRequestEvent = "merge_request_event"
sourceToApiRep SourceExternalPullRequestEvent = "external_pull_request_event"
sourceToApiRep SourceParentPipeline = "parent_pipeline"
sourceToApiRep SourceOndemandDastScan = "ondemand_dast_scan"
sourceToApiRep SourceOndemandDastValidation = "ondemand_dast_validation"

instance FromJSON Source where
  parseJSON = withText "Source" $ \text -> maybe (error $ "can't parse '" <> text <> "' into Source") pure (inverseMap sourceToApiRep text)

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

data SchedulesApi m a where
  GetSchedules :: ProjectId -> SchedulesApi m (Either UpdateError [Schedule])

makeSem ''SchedulesApi
