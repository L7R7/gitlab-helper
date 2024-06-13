{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effects
  ( Writer (..),
    write,
    Timer (..),
    getCurrentTime,
    GroupsApi (..),
    getAllGroups,
    UsersApi (..),
    getAllUsers,
    UserId (..),
    User (..),
    ProjectsApi (..),
    getProjectsForGroup,
    getProjectsForUser,
    getProject,
    hasCi,
    setMergeMethod,
    MergeRequestApi (..),
    getOpenMergeRequests,
    getOpenMergeRequestsForGroup,
    enableSourceBranchDeletionAfterMrMerge,
    setSuccessfulPipelineRequirementForMerge,
    unsetSuccessfulPipelineRequirementForMerge,
    setResolvedDiscussionsRequirementForMerge,
    mergeMergeRequest,
    rebaseMergeRequest,
    BranchesApi (..),
    getBranches,
    PipelinesApi (..),
    getPipeline,
    getSuccessfulPipelines,
    getSuccessfulPushPipelines,
    SchedulesApi (..),
    getSchedules,
    Duration (..),
    Sha (..),
    Source (..),
    Pipeline (..),
    PipelineId (..),
    CompactPipeline (..),
    Schedule (..),
  )
where

import Autodocodec
import Config.Types (AuthorIs, SearchTerm, Year)
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific
import qualified Data.Text as T hiding (partition)
import Data.Time (UTCTime)
import Gitlab.Branch
import Gitlab.Client (UpdateError)
import Gitlab.Group
import Gitlab.Lib (Id (..), Ref (..))
import Gitlab.MergeRequest
import Gitlab.Project
import Network.URI
import Polysemy
import Relude

data Schedule = Schedule
  { scheduleId :: Int,
    scheduleDescription :: Text,
    scheduleCron :: Text,
    scheduleCronTimezone :: Text,
    scheduleNextRunAt :: UTCTime,
    scheduleActive :: Bool,
    scheduleOwner :: Text
  }
  deriving (FromJSON) via (Autodocodec Schedule)

instance HasCodec Schedule where
  codec =
    object "Schedule"
      $ Schedule
      <$> requiredField' "id"
      .= scheduleId
      <*> requiredField' "description"
      .= scheduleDescription
      <*> requiredField' "cron"
      .= scheduleCron
      <*> requiredField' "cron_timezone"
      .= scheduleCronTimezone
      <*> requiredField' "next_run_at"
      .= scheduleNextRunAt
      <*> requiredField' "active"
      .= scheduleActive
      <*> (ownerName <$> (requiredField' "owner" .= (Owner . scheduleOwner)))

newtype Owner = Owner {ownerName :: Text}

instance HasCodec Owner where
  codec = object "Owner" $ Owner <$> requiredField' "name" .= ownerName

data Writer m a where
  Write :: Text -> Writer m ()

makeSem ''Writer

data Timer m a where
  GetCurrentTime :: Timer m UTCTime

makeSem ''Timer

newtype UserId = UserId Int
  deriving newtype (Show)
  deriving (FromJSON) via (Autodocodec UserId)

instance HasCodec UserId where
  codec = dimapCodec UserId (\(UserId i) -> i) codec

newtype User = User
  { userId :: UserId
  }
  deriving (FromJSON) via (Autodocodec User)

instance HasCodec User where
  codec = object "User" $ User <$> requiredField' "id" .= userId

data UsersApi m a where
  GetAllUsers :: UsersApi m (Either UpdateError [User])

makeSem ''UsersApi

data GroupsApi m a where
  GetAllGroups :: GroupsApi m (Either UpdateError [Group])

makeSem ''GroupsApi

data ProjectsApi m a where
  GetProjectsForGroup :: Id Group -> ProjectsApi m (Either UpdateError [Project])
  GetProjectsForUser :: UserId -> ProjectsApi m (Either UpdateError [Project])
  GetProject :: Id Project -> ProjectsApi m (Either UpdateError Project)
  HasCi :: Id Project -> Ref -> ProjectsApi m (Either UpdateError Bool)
  SetMergeMethod :: Id Project -> MergeMethod -> ProjectsApi m (Either UpdateError ())

makeSem ''ProjectsApi

data MergeRequestApi m a where
  GetOpenMergeRequests :: Id Project -> Maybe AuthorIs -> MergeRequestApi m (Either UpdateError [MergeRequest])
  GetOpenMergeRequestsForGroup :: Id Group -> Maybe AuthorIs -> Maybe SearchTerm -> MergeRequestApi m (Either UpdateError [MergeRequest])
  EnableSourceBranchDeletionAfterMrMerge :: Id Project -> MergeRequestApi m (Either UpdateError ())
  SetSuccessfulPipelineRequirementForMerge :: Id Project -> MergeRequestApi m (Either UpdateError ())
  UnsetSuccessfulPipelineRequirementForMerge :: Id Project -> MergeRequestApi m (Either UpdateError ())
  SetResolvedDiscussionsRequirementForMerge :: Id Project -> MergeRequestApi m (Either UpdateError ())
  MergeMergeRequest :: Id Project -> Id MergeRequest -> MergeRequestApi m (Either UpdateError ())
  RebaseMergeRequest :: Id Project -> Id MergeRequest -> MergeRequestApi m (Either UpdateError ())

makeSem ''MergeRequestApi

data BranchesApi m a where
  GetBranches :: Id Project -> BranchesApi m (Either UpdateError [Branch])

makeSem ''BranchesApi

newtype PipelineId = PipelineId {getPipelineId :: Int}
  deriving newtype (Eq, Num, Ord, Show)
  deriving (FromJSON) via (Autodocodec PipelineId)

instance HasCodec PipelineId where
  codec = dimapCodec PipelineId getPipelineId boundedIntegralCodec

newtype Duration = Duration {getDuration :: Scientific}
  deriving newtype (Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Duration)

instance ToText Duration where
  toText (Duration duration) = show duration

instance HasCodec Duration where
  codec = dimapCodec Duration getDuration scientificCodec

newtype Sha = Sha {getSha :: T.Text}
  deriving newtype (Show)
  deriving (FromJSON) via (Autodocodec Sha)

instance HasCodec Sha where
  codec = dimapCodec Sha getSha textCodec

data Pipeline = Pipeline
  { pipelineId :: PipelineId,
    pipelineSha :: Sha,
    pipelineDuration :: Maybe Duration,
    pipelineQueuedDuration :: Maybe Duration,
    pipelineCreatedAt :: UTCTime,
    pipelineWebUrl :: URI,
    pipelineSource :: Source
  }
  deriving stock (Show)
  deriving (FromJSON) via (Autodocodec Pipeline)

instance HasCodec Pipeline where
  codec =
    object "Pipeline"
      $ Pipeline
      <$> requiredField' "id"
      .= pipelineId
      <*> requiredField' "sha"
      .= pipelineSha
      <*> requiredField' "duration"
      .= pipelineDuration
      <*> requiredField' "queued_duration"
      .= pipelineQueuedDuration
      <*> requiredField' "created_at"
      .= pipelineCreatedAt
      <*> requiredField' "web_url"
      .= pipelineWebUrl
      <*> requiredField' "source"
      .= pipelineSource

data Source = SourcePush | SourceWeb | SourceTrigger | SourceSchedule | SourceApi | SourceExternal | SourcePipeline | SourceChat | SourceWebide | SourceMergeRequestEvent | SourceExternalPullRequestEvent | SourceParentPipeline | SourceOndemandDastScan | SourceOndemandDastValidation
  deriving stock (Bounded, Enum, Eq, Show)
  deriving (FromJSON) via (Autodocodec Source)

instance HasCodec Source where
  codec = bimapCodec (maybeToRight "can't parse source" . inverseMap sourceToApiRep) show textCodec

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

data CompactPipeline = CompactPipeline
  { compactPipelineId :: PipelineId,
    compactPipelineSha :: Sha
  }
  deriving (FromJSON) via (Autodocodec CompactPipeline)

instance HasCodec CompactPipeline where
  codec =
    object "CompactPipeline"
      $ CompactPipeline
      <$> requiredField' "id"
      .= compactPipelineId
      <*> requiredField' "sha"
      .= compactPipelineSha

data PipelinesApi m a where
  GetPipeline :: Id Project -> PipelineId -> PipelinesApi m (Either UpdateError Pipeline)
  GetSuccessfulPipelines :: Id Project -> Ref -> PipelinesApi m (Either UpdateError [CompactPipeline])
  GetSuccessfulPushPipelines :: Year -> Id Project -> Ref -> PipelinesApi m (Either UpdateError [CompactPipeline])

makeSem ''PipelinesApi

data SchedulesApi m a where
  GetSchedules :: Id Project -> SchedulesApi m (Either UpdateError [Schedule])

makeSem ''SchedulesApi
