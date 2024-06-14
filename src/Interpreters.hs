{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters (usersApiToIO, groupsApiToIO, projectsApiToIO, mergeRequestApiToIO, branchesApiToIO, pipelinesApiToIO, schedulesApiToIO, runM) where

import Burrito
import Config.Types (AuthorIs (..), MergeCiOption (..), SearchTerm (..), Year (..))
import Data.Aeson hiding (Value)
import Effects
import Gitlab.Client
import Gitlab.Lib (Ref (..))
import Gitlab.Project
import Network.HTTP.Client.Conduit (RequestBody (..))
import Network.HTTP.Simple
import Network.HTTP.Types (Status (statusCode), hContentType)
import Polysemy
import Relude hiding (group)

usersApiToIO :: (Member (Embed IO) r) => BaseUrl -> ApiToken -> InterpreterFor UsersApi r
usersApiToIO baseUrl apiToken = interpret $ \case
  GetAllUsers -> do
    let template = [uriTemplate|/api/v4/users|]
    embed $ fetchDataPaginated apiToken baseUrl template []

groupsApiToIO :: (Member (Embed IO) r) => BaseUrl -> ApiToken -> InterpreterFor GroupsApi r
groupsApiToIO baseUrl apiToken = interpret $ \case
  GetAllGroups -> do
    let template = [uriTemplate|/api/v4/groups?all_available=true|]
    embed $ fetchDataPaginated apiToken baseUrl template []

projectsApiToIO :: (Member (Embed IO) r) => BaseUrl -> ApiToken -> InterpreterFor ProjectsApi r
projectsApiToIO baseUrl apiToken = interpret $ \case
  GetProjectsForGroup gId -> do
    let template = [uriTemplate|/api/v4/groups/{groupId}/projects?include_subgroups=true&archived=false&with_shared=false|]
    embed $ fetchDataPaginated apiToken baseUrl template [("groupId", (stringValue . show) gId)]
  GetProjectsForUser uId -> do
    let template = [uriTemplate|/api/v4/users/{userId}/projects?archived=false|]
    embed $ fetchDataPaginated apiToken baseUrl template [("userId", (stringValue . show) uId)]
  GetProject project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}|]
    embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) project)]
  HasCi project ref -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/repository/files/.gitlab-ci.yml?ref={ref}|]
    response <- embed $ headRequest baseUrl apiToken id template [("projectId", (stringValue . show) project), ("ref", (stringValue . (\(Ref txt) -> toString txt)) ref)]
    pure $ (200 ==) . statusCode <$> response
  SetMergeMethod project mm -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}?merge_method={merge_method}|]
        toAPIValue Merge = "merge"
        toAPIValue RebaseMerge = "rebase_merge"
        toAPIValue FastForward = "ff"
    embed $ void <$> fetchData' @Project baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project), ("merge_method", stringValue (toAPIValue mm))]

mergeRequestApiToIO :: (Member (Embed IO) r) => BaseUrl -> ApiToken -> InterpreterFor MergeRequestApi r
mergeRequestApiToIO baseUrl apiToken = interpret $ \case
  GetOpenMergeRequests project maybeAuthorIs -> do
    case maybeAuthorIs of
      Nothing -> do
        let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests?state=opened|]
        embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) project)]
      Just (AuthorIs i) -> do
        let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests?state=opened&author_id={authorId}|]
        embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) project), ("authorId", (stringValue . show) i)]
  GetOpenMergeRequestsForGroup group maybeAuthorIs maybeSearchTerm -> do
    let template = [uriTemplate|/api/v4/groups/{groupId}/merge_requests?state=opened{&author_id,search}|]
    embed
      $ fetchDataPaginated
        apiToken
        baseUrl
        template
        ( mconcat
            [ [("groupId", (stringValue . show) group)],
              foldMap (\(AuthorIs i) -> [("author_id", (stringValue . show) i)]) maybeAuthorIs,
              foldMap (\(SearchTerm s) -> [("search", stringValue s)]) maybeSearchTerm
            ]
        )
  EnableSourceBranchDeletionAfterMrMerge project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}?remove_source_branch_after_merge=true|]
    embed $ void <$> fetchData' @Project baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project)]
  SetSuccessfulPipelineRequirementForMerge project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}?only_allow_merge_if_pipeline_succeeds=true|]
    embed $ void <$> fetchData' @Project baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project)]
  UnsetSuccessfulPipelineRequirementForMerge project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}?only_allow_merge_if_pipeline_succeeds=false|]
    embed $ void <$> fetchData' @Project baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project)]
  SetResolvedDiscussionsRequirementForMerge project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}?only_allow_merge_if_all_discussions_are_resolved=true|]
    embed $ void <$> fetchData' @Project baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project)]
  MergeMergeRequest project mrId skipCiOption -> do
    let skipCiOptionToParam PipelineMustSucceed = "true"
        skipCiOptionToParam SkipCi = "false"
        template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests/{mergeRequestId}/merge?should_remove_source_branch=true&merge_when_pipeline_succeeds={pipelineMustSucceed}|]
    embed $ void <$> fetchData' @Object baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project), ("mergeRequestId", (stringValue . show) mrId), ("pipelineMustSucceed", stringValue $ skipCiOptionToParam skipCiOption)]
  RebaseMergeRequest project mrId -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests/{mergeRequestId}/rebase|]
    -- Right (fromList [("rebase_in_progress",Bool True)])
    embed $ void <$> fetchData' @Object baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project), ("mergeRequestId", (stringValue . show) mrId)]
  SetMergeRequestTitle project mrId newTitle -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests/{mergeRequestId}|]
        setTitle = setRequestHeader hContentType ["application/x-www-form-urlencoded"] . setRequestBody (RequestBodyBS (encodeUtf8 $ "title=" <> newTitle))
    embed $ void <$> fetchData' @Object baseUrl apiToken (setTitle . setRequestMethod "PUT") template [("projectId", (stringValue . show) project), ("mergeRequestId", (stringValue . show) mrId)]

branchesApiToIO :: (Member (Embed IO) r) => BaseUrl -> ApiToken -> InterpreterFor BranchesApi r
branchesApiToIO baseUrl apiToken = interpret $ \case
  GetBranches project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/repository/branches|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) project)]

pipelinesApiToIO :: (Member (Embed IO) r) => BaseUrl -> ApiToken -> InterpreterFor PipelinesApi r
pipelinesApiToIO baseUrl apiToken = interpret $ \case
  GetSuccessfulPushPipelines (Year year) pId (Ref ref) -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref={ref}&status={status}&updated_after={year0}-01-01T00:00:00Z&updated_before={year1}-01-01T00:00:00Z&source=push|]
    embed
      $ fetchDataPaginated
        apiToken
        baseUrl
        template
        [ ("projectId", (stringValue . show) pId),
          ("ref", (stringValue . toString) ref),
          ("status", stringValue "success"),
          ("year0", (stringValue . show) year),
          ("year1", (stringValue . show) (year + 1))
        ]

schedulesApiToIO :: (Member (Embed IO) r) => BaseUrl -> ApiToken -> InterpreterFor SchedulesApi r
schedulesApiToIO baseUrl apiToken = interpret $ \case
  GetSchedules project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipeline_schedules|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) project)]
