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

module Effects
  ( Writer (..),
    write,
    Timer (..),
    getCurrentTime,
    GroupsApi (..),
    getAllGroups,
    Group (..),
    UsersApi (..),
    getAllUsers,
    UserId (..),
    User (..),
    ProjectsApi (..),
    getProjectsForGroup,
    getProjectsForUser,
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

import Autodocodec
import Config.Types hiding (groupId)
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Scientific
import qualified Data.Text as T hiding (partition)
import Data.Time (UTCTime)
import Network.HTTP.Client.Conduit (HttpException)
import Network.HTTP.Simple (JSONException)
import Network.URI
import Polysemy
import Relude
import qualified Text.Show

newtype ProjectId = ProjectId {getProjectId :: Int}
  deriving newtype (Show)
  deriving (FromJSON) via (Autodocodec ProjectId)

instance HasCodec ProjectId where
  codec = dimapCodec ProjectId getProjectId boundedIntegralCodec

newtype ProjectName = ProjectName {getProjectName :: Text}
  deriving newtype (Eq, Ord)
  deriving (FromJSON) via (Autodocodec ProjectName)

instance Show ProjectName where
  show = toString . getProjectName

instance HasCodec ProjectName where
  codec = dimapCodec ProjectName getProjectName textCodec

newtype Ref = Ref {getRef :: T.Text}
  deriving newtype (Show)
  deriving (FromJSON) via (Autodocodec Ref)

instance HasCodec Ref where
  codec = dimapCodec Ref getRef textCodec

data Project = Project
  { projectId :: ProjectId,
    name :: ProjectName,
    mergeRequestsEnabled :: Bool,
    mergeMethod :: MergeMethod,
    defaultBranch :: Maybe Ref,
    removeSourceBranchAfterMerge :: Maybe Bool,
    onlyAllowMergeIfPipelineSucceeds :: Maybe Bool,
    onlyAllowMergeIfAllDiscussionsAreResolved :: Maybe Bool,
    autoCancelPendingPipelines :: EnabledDisabled,
    pathWithNamespace :: Text,
    sshUrlToRepo :: Text
  }
  deriving (FromJSON) via (Autodocodec Project)

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

instance HasCodec Project where
  codec =
    object "Project" $
      Project
        <$> requiredField' "id"
        .= projectId
        <*> requiredField' "name"
        .= name
        <*> requiredField' "merge_requests_enabled"
        .= mergeRequestsEnabled
        <*> requiredField' "merge_method"
        .= mergeMethod
        <*> optionalField' "default_branch"
        .= defaultBranch
        <*> requiredField' "remove_source_branch_after_merge"
        .= removeSourceBranchAfterMerge
        <*> requiredField' "only_allow_merge_if_pipeline_succeeds"
        .= onlyAllowMergeIfPipelineSucceeds
        <*> requiredField' "only_allow_merge_if_all_discussions_are_resolved"
        .= onlyAllowMergeIfAllDiscussionsAreResolved
        <*> requiredField' "auto_cancel_pending_pipelines"
        .= autoCancelPendingPipelines
        <*> requiredField' "path_with_namespace"
        .= pathWithNamespace
        <*> requiredField' "ssh_url_to_repo"
        .= sshUrlToRepo

instance HasCodec URI where
  codec = bimapCodec (maybeToRight "can't parse URI" . parseURI) show stringCodec

data EnabledDisabled = Enabled | Disabled
  deriving stock (Eq, Show)
  deriving (FromJSON) via (Autodocodec EnabledDisabled)

instance HasCodec EnabledDisabled where
  codec = stringConstCodec $ (Enabled, "enabled") :| [(Disabled, "disabled")]

data MergeMethod = Merge | RebaseMerge | FastForward
  deriving stock (Eq, Show)

instance HasCodec MergeMethod where
  codec = stringConstCodec $ (Merge, "merge") :| [(RebaseMerge, "rebase_merge"), (FastForward, "ff")]

data MergeRequest = MergeRequest
  { mergeRequestId :: MergeRequestId,
    wip :: Bool,
    conflicts :: Bool,
    createdAt :: UTCTime,
    webUrl :: URI
  }
  deriving stock (Show)
  deriving (FromJSON) via (Autodocodec MergeRequest)

instance HasCodec MergeRequest where
  codec =
    object "MergeRequest" $
      MergeRequest
        <$> requiredField' "id"
        .= mergeRequestId
        <*> requiredField' "work_in_progress"
        .= wip
        <*> requiredField' "has_conflicts"
        .= conflicts
        <*> requiredField' "created_at"
        .= createdAt
        <*> requiredField' "web_url"
        .= webUrl

newtype MergeRequestId = MergeRequestId {getMergeRequestId :: Int}
  deriving newtype (Show)
  deriving (FromJSON) via (Autodocodec MergeRequestId)

instance HasCodec MergeRequestId where
  codec = dimapCodec MergeRequestId getMergeRequestId boundedIntegralCodec

data Branch = Branch
  { branchName :: T.Text,
    branchMerged :: Bool,
    branchProtected :: Bool,
    branchDefault :: Bool,
    branchWebUrl :: URI,
    branchCommittedDate :: UTCTime
  }
  deriving stock (Show)
  deriving (FromJSON) via (Autodocodec Branch)

instance HasCodec Branch where
  codec =
    object "Branch" $
      Branch
        <$> requiredField' "name"
        .= branchName
        <*> requiredField' "merged"
        .= branchMerged
        <*> requiredField' "protected"
        .= branchProtected
        <*> requiredField' "default"
        .= branchDefault
        <*> requiredField' "web_url"
        .= branchWebUrl
        <*> (committedDate <$> (requiredField' "commit" .= (Commit . branchCommittedDate)))

newtype Commit = Commit {committedDate :: UTCTime}

instance HasCodec Commit where
  codec = object "Commit" $ Commit <$> requiredField' "committed_date" .= committedDate

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
    object "Schedule" $
      Schedule
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

data UpdateError
  = HttpError HttpException
  | ExceptionError SomeException
  | ConversionError JSONException
  | ParseUrlError Text
  deriving stock (Show)

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

newtype Group = Group
  { groupId :: GroupId
  }
  deriving (FromJSON) via (Autodocodec Group)

instance HasCodec Group where
  codec = object "Group" $ Group <$> requiredField' "id" .= groupId

data GroupsApi m a where
  GetAllGroups :: GroupsApi m (Either UpdateError [Group])

makeSem ''GroupsApi

data ProjectsApi m a where
  GetProjectsForGroup :: GroupId -> ProjectsApi m (Either UpdateError [Project])
  GetProjectsForUser :: UserId -> ProjectsApi m (Either UpdateError [Project])
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
    object "Pipeline" $
      Pipeline
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
    object "CompactPipeline" $
      CompactPipeline
        <$> requiredField' "id"
        .= compactPipelineId
        <*> requiredField' "sha"
        .= compactPipelineSha

data PipelinesApi m a where
  GetPipeline :: ProjectId -> PipelineId -> PipelinesApi m (Either UpdateError Pipeline)
  GetSuccessfulPipelines :: ProjectId -> Ref -> PipelinesApi m (Either UpdateError [CompactPipeline])

makeSem ''PipelinesApi

data SchedulesApi m a where
  GetSchedules :: ProjectId -> SchedulesApi m (Either UpdateError [Schedule])

makeSem ''SchedulesApi
