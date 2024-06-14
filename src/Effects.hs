{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Effects
  ( write,
    getCurrentTime,

    -- * Groups
    getAllGroups,

    -- * Users
    getAllUsers,
    UserId (..),
    User (..),

    -- * Projects
    getProjectsForGroup,
    getProjectsForUser,
    getProject,
    hasCi,
    setMergeMethod,

    -- * MergeRequest
    getOpenMergeRequests,
    getOpenMergeRequestsForGroup,
    enableSourceBranchDeletionAfterMrMerge,
    setSuccessfulPipelineRequirementForMerge,
    unsetSuccessfulPipelineRequirementForMerge,
    setResolvedDiscussionsRequirementForMerge,
    mergeMergeRequest,
    rebaseMergeRequest,
    setMergeRequestTitle,

    -- * Branches
    getBranches,

    -- * Pipelines
    getPipeline,
    getSuccessfulPipelines,
    getSuccessfulPushPipelines,

    -- * Schedules
    getSchedules,

    -- * Types
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
import Burrito
import Config.Types (AuthorIs (..), Config (..), MergeCiOption (..), SearchTerm (..), Year (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (Object)
import Data.Scientific
import qualified Data.Text as T hiding (partition)
import Data.Time (UTCTime)
import qualified Data.Time
import Gitlab.Branch
import Gitlab.Group hiding (groupId)
import Gitlab.Lib (Id (..), Ref (..))
import Gitlab.MergeRequest
import Gitlab.Project
import GitlabUtils
import Network.HTTP.Client.Conduit (RequestBody (..))
import Network.HTTP.Simple (setRequestBody, setRequestHeader, setRequestMethod)
import Network.HTTP.Types (Status (..), hContentType)
import Network.URI
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

write :: Text -> ReaderT Config IO ()
write = putStrLn . toString

getCurrentTime :: ReaderT Config IO UTCTime
getCurrentTime = liftIO Data.Time.getCurrentTime

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

getAllUsers :: ReaderT Config IO (Either UpdateError [User])
getAllUsers = fetchDataPaginated [uriTemplate|/api/v4/users|] []

getAllGroups :: ReaderT Config IO (Either UpdateError [Group])
getAllGroups = fetchDataPaginated [uriTemplate|/api/v4/groups?all_available=true|] []

getProjectsForGroup :: ReaderT Config IO (Either UpdateError [Project])
getProjectsForGroup = do
  gId <- asks groupId
  let template = [uriTemplate|/api/v4/groups/{groupId}/projects?include_subgroups=true&archived=false&with_shared=false|]
  fetchDataPaginated template [("groupId", (stringValue . show) gId)]

getProjectsForUser :: UserId -> ReaderT Config IO (Either UpdateError [Project])
getProjectsForUser uId = fetchDataPaginated [uriTemplate|/api/v4/users/{userId}/projects?archived=false|] [("userId", (stringValue . show) uId)]

getProject :: Id Project -> ReaderT Config IO (Either UpdateError Project)
getProject project = fetchData [uriTemplate|/api/v4/projects/{projectId}|] [("projectId", (stringValue . show) project)]

hasCi :: Id Project -> Ref -> ReaderT Config IO (Either UpdateError Bool)
hasCi project ref = do
  let template = [uriTemplate|/api/v4/projects/{projectId}/repository/files/.gitlab-ci.yml?ref={ref}|]
  response <- headRequest id template [("projectId", (stringValue . show) project), ("ref", (stringValue . (\(Ref txt) -> toString txt)) ref)]
  pure $ (200 ==) . statusCode <$> response

setMergeMethod :: Id Project -> MergeMethod -> ReaderT Config IO (Either UpdateError ())
setMergeMethod project mm = do
  let template = [uriTemplate|/api/v4/projects/{projectId}?merge_method={merge_method}|]
      toAPIValue Merge = "merge"
      toAPIValue RebaseMerge = "rebase_merge"
      toAPIValue FastForward = "ff"
  void <$> fetchData' @Project (setRequestMethod "PUT") template [("projectId", (stringValue . show) project), ("merge_method", stringValue (toAPIValue mm))]

getOpenMergeRequests :: Id Project -> Maybe AuthorIs -> ReaderT Config IO (Either UpdateError [MergeRequest])
getOpenMergeRequests project maybeAuthorIs = do
  case maybeAuthorIs of
    Nothing -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests?state=opened|]
      fetchDataPaginated template [("projectId", (stringValue . show) project)]
    Just (AuthorIs i) -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests?state=opened&author_id={authorId}|]
      fetchDataPaginated template [("projectId", (stringValue . show) project), ("authorId", (stringValue . show) i)]

getOpenMergeRequestsForGroup :: Maybe AuthorIs -> Maybe SearchTerm -> ReaderT Config IO (Either UpdateError [MergeRequest])
getOpenMergeRequestsForGroup maybeAuthorIs maybeSearchTerm = do
  grp <- asks groupId
  let template = [uriTemplate|/api/v4/groups/{groupId}/merge_requests?state=opened{&author_id,search}|]
  fetchDataPaginated
    template
    ( mconcat
        [ [("groupId", (stringValue . show) grp)],
          foldMap (\(AuthorIs i) -> [("author_id", (stringValue . show) i)]) maybeAuthorIs,
          foldMap (\(SearchTerm s) -> [("search", stringValue s)]) maybeSearchTerm
        ]
    )

enableSourceBranchDeletionAfterMrMerge :: Id Project -> ReaderT Config IO (Either UpdateError ())
enableSourceBranchDeletionAfterMrMerge project =
  void <$> fetchData' @Project (setRequestMethod "PUT") [uriTemplate|/api/v4/projects/{projectId}?remove_source_branch_after_merge=true|] [("projectId", (stringValue . show) project)]

setSuccessfulPipelineRequirementForMerge :: Id Project -> ReaderT Config IO (Either UpdateError ())
setSuccessfulPipelineRequirementForMerge project =
  void <$> fetchData' @Project (setRequestMethod "PUT") [uriTemplate|/api/v4/projects/{projectId}?only_allow_merge_if_pipeline_succeeds=true|] [("projectId", (stringValue . show) project)]

unsetSuccessfulPipelineRequirementForMerge :: Id Project -> ReaderT Config IO (Either UpdateError ())
unsetSuccessfulPipelineRequirementForMerge project =
  void <$> fetchData' @Project (setRequestMethod "PUT") [uriTemplate|/api/v4/projects/{projectId}?only_allow_merge_if_pipeline_succeeds=false|] [("projectId", (stringValue . show) project)]

setResolvedDiscussionsRequirementForMerge :: Id Project -> ReaderT Config IO (Either UpdateError ())
setResolvedDiscussionsRequirementForMerge project =
  void
    <$> fetchData' @Project
      (setRequestMethod "PUT")
      [uriTemplate|/api/v4/projects/{projectId}?only_allow_merge_if_all_discussions_are_resolved=true|]
      [("projectId", (stringValue . show) project)]

mergeMergeRequest :: Id Project -> Id MergeRequest -> MergeCiOption -> ReaderT Config IO (Either UpdateError ())
mergeMergeRequest project mrId skipCiOption = do
  let skipCiOptionToParam PipelineMustSucceed = "true"
      skipCiOptionToParam SkipCi = "false"
  void
    <$> fetchData' @Object
      (setRequestMethod "PUT")
      [uriTemplate|/api/v4/projects/{projectId}/merge_requests/{mergeRequestId}/merge?should_remove_source_branch=true&merge_when_pipeline_succeeds={pipelineMustSucceed}|]
      [("projectId", (stringValue . show) project), ("mergeRequestId", (stringValue . show) mrId), ("pipelineMustSucceed", stringValue $ skipCiOptionToParam skipCiOption)]

rebaseMergeRequest :: Id Project -> Id MergeRequest -> ReaderT Config IO (Either UpdateError ())
rebaseMergeRequest project mrId =
  -- Right (fromList [("rebase_in_progress",Bool True)])
  void
    <$> fetchData' @Object
      (setRequestMethod "PUT")
      [uriTemplate|/api/v4/projects/{projectId}/merge_requests/{mergeRequestId}/rebase|]
      [("projectId", (stringValue . show) project), ("mergeRequestId", (stringValue . show) mrId)]

setMergeRequestTitle :: Id Project -> Id MergeRequest -> Text -> ReaderT Config IO (Either UpdateError ())
setMergeRequestTitle project mrId newTitle = do
  let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests/{mergeRequestId}|]
      setTitle = setRequestHeader hContentType ["application/x-www-form-urlencoded"] . setRequestBody (RequestBodyBS (encodeUtf8 $ "title=" <> newTitle))
  void <$> fetchData' @Object (setTitle . setRequestMethod "PUT") template [("projectId", (stringValue . show) project), ("mergeRequestId", (stringValue . show) mrId)]

getBranches :: Id Project -> ReaderT Config IO (Either UpdateError [Branch])
getBranches project = fetchDataPaginated [uriTemplate|/api/v4/projects/{projectId}/repository/branches|] [("projectId", (stringValue . show) project)]

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
  codec = bimapCodec (maybeToRight "can't parse source" . inverseMap sourceToApiRep) sourceToApiRep textCodec

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

getPipeline :: Id Project -> PipelineId -> ReaderT Config IO (Either UpdateError Pipeline)
getPipeline project pipeline = do
  let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
  fetchData template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)]

getSuccessfulPipelines :: Id Project -> Ref -> ReaderT Config IO (Either UpdateError [CompactPipeline])
getSuccessfulPipelines pId (Ref ref) = do
  let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref={ref}&status={status}&updated_after=2021-01-06T00:00:00Z&source=push|]
  fetchDataPaginated template [("projectId", (stringValue . show) pId), ("ref", (stringValue . toString) ref), ("status", stringValue "success")]

getSuccessfulPushPipelines :: Year -> Id Project -> Ref -> ReaderT Config IO (Either UpdateError [CompactPipeline])
getSuccessfulPushPipelines (Year year) pId (Ref ref) = do
  let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref={ref}&status={status}&updated_after={year0}-01-01T00:00:00Z&updated_before={year1}-01-01T00:00:00Z&source=push|]
  fetchDataPaginated
    template
    [ ("projectId", (stringValue . show) pId),
      ("ref", (stringValue . toString) ref),
      ("status", stringValue "success"),
      ("year0", (stringValue . show) year),
      ("year1", (stringValue . show) (year + 1))
    ]

getSchedules :: Id Project -> ReaderT Config IO (Either UpdateError [Schedule])
getSchedules project = fetchDataPaginated [uriTemplate|/api/v4/projects/{projectId}/pipeline_schedules|] [("projectId", (stringValue . show) project)]
