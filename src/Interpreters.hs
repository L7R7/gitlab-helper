{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters (projectsApiToIO, mergeRequestApiToIO, branchesApiToIO, pipelinesApiToIO, runM) where

import Burrito
import Config (ApiToken (..), BaseUrl)
import Control.Exception.Base (try)
import Control.Lens (Lens', Prism', Traversal', filtered, lens, prism', set, _1, _2)
import Control.Monad (join)
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Either.Combinators (mapLeft, rightToMaybe)
import Data.List (find)
import Effects
import Network.HTTP.Client.Conduit (HttpExceptionContent, requestFromURI, requestHeaders, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)
import Polysemy

projectsApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (ProjectsApi ': r) a -> Sem r a
projectsApiToIO baseUrl apiToken = interpret $ \case
  GetProjects groupId -> do
    let template = [uriTemplate|/api/v4/groups/{groupId}/projects?include_subgroups=true&archived=false|]
    embed $ fetchDataPaginated apiToken baseUrl template [("groupId", (stringValue . show) groupId)]
  GetProject projectId -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}|]
    embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) projectId)]

mergeRequestApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (MergeRequestApi ': r) a -> Sem r a
mergeRequestApiToIO baseUrl apiToken = interpret $ \case
  GetOpenMergeRequests projectId -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests?state=opened|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) projectId)]

branchesApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (BranchesApi ': r) a -> Sem r a
branchesApiToIO baseUrl apiToken = interpret $ \case
  GetBranches projectId -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/repository/branches|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) projectId)]

pipelinesApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (PipelinesApi ': r) a -> Sem r a
pipelinesApiToIO baseUrl apiToken = interpret $ \case
  GetPipeline projectId pipelineId -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
    embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) projectId), ("pipelineId", (stringValue . show) pipelineId)]
  GetSuccessfulPipelines projectId ref -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref=master&status={status}&updated_after=2019-06-09T08:00:00Z|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) projectId), ("ref", (stringValue . show) ref), ("status", stringValue "success")]

fetchDataPaginated :: (FromJSON a) => ApiToken -> BaseUrl -> Template -> [(String, Value)] -> IO (Either UpdateError [a])
fetchDataPaginated apiToken baseUrl template vars = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchDataPaginated' apiToken template request []

fetchDataPaginated' :: (FromJSON a) => ApiToken -> Template -> Request -> [a] -> IO (Either UpdateError [a])
fetchDataPaginated' apiToken template request acc = do
  result <- try $ do
    response <- httpJSONEither (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken template req (as <> acc)) next
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

fetchData :: (FromJSON a) => BaseUrl -> ApiToken -> Template -> [(String, Value)] -> IO (Either UpdateError a)
fetchData baseUrl apiToken template vars = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    Left invalidUrl -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchData' apiToken request

fetchData' :: (FromJSON a) => ApiToken -> Request -> IO (Either UpdateError a)
fetchData' apiToken request = do
  result <- try (mapLeft ConversionError . getResponseBody <$> httpJSONEither (setTimeout $ addToken apiToken request))
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

setTimeout :: Request -> Request
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

addToken :: ApiToken -> Request -> Request
addToken apiToken = setRequestHeader "PRIVATE-TOKEN" [coerce apiToken]

parseNextRequest :: Response a -> Maybe Request
parseNextRequest response = parseNextHeader response >>= rightToMaybe . requestFromURI

parseNextHeader :: Response a -> Maybe URI
parseNextHeader response = href <$> find isNextLink ((parseLinkHeaderBS <$> getResponseHeader "link" response) >>= concat)

isNextLink :: Link -> Bool
isNextLink (Link _ [(Rel, "next")]) = True
isNextLink _ = False

removeApiTokenFromUpdateError :: UpdateError -> UpdateError
removeApiTokenFromUpdateError (HttpError httpException) = HttpError (removeApiTokenFromHttpException httpException)
removeApiTokenFromUpdateError (ConversionError jsonException) = ConversionError (removeApiTokenFromJsonException jsonException)

removeApiTokenFromHttpException :: HttpException -> HttpException
removeApiTokenFromHttpException = set (reqPrism . _1 . headers . tokenHeader) "xxxxx"

reqPrism :: Prism' HttpException (Request, HttpExceptionContent)
reqPrism = prism' (uncurry HttpExceptionRequest) extract
  where
    extract (HttpExceptionRequest request reason) = Just (request, reason)
    extract _ = Nothing

tokenHeader :: Traversal' RequestHeaders ByteString
tokenHeader = traverse . filtered (\h -> fst h == privateToken) . _2

privateToken :: HeaderName
privateToken = "PRIVATE-TOKEN"

headers :: Lens' Request RequestHeaders
headers = lens getter setter
  where
    getter = requestHeaders
    setter r h = r {requestHeaders = h}

removeApiTokenFromJsonException :: JSONException -> JSONException
removeApiTokenFromJsonException (JSONParseException request response parseError) = JSONParseException (removeApiTokenFromRequest request) response parseError
removeApiTokenFromJsonException (JSONConversionException request response s) = JSONConversionException (removeApiTokenFromRequest request) response s

removeApiTokenFromRequest :: Request -> Request
removeApiTokenFromRequest = set (headers . tokenHeader) "xxxxx"
