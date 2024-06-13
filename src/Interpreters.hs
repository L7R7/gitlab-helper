{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters (projectsApiToIO, mergeRequestApiToIO, branchesApiToIO, pipelinesApiToIO, schedulesApiToIO, writeToFileToIO, runM) where

import Burrito
import Config.Types (ApiToken (..), BaseUrl)
import Control.Exception.Base (try)
import Control.Lens (Lens', Prism', Traversal', filtered, lens, prism', set, _1, _2)
import Data.Aeson.Types (FromJSON)
import qualified Data.DateTime as DT
import Data.Either.Combinators (mapLeft)
import Effects
import Graphics.Vega.VegaLite
import Network.HTTP.Client.Conduit (HttpExceptionContent, requestFromURI, requestHeaders, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)
import Pipelines hiding (id)
import Polysemy
import Relude

writeToFileToIO :: Member (Embed IO) r => Sem (WriteToFile ': r) a -> Sem r a
writeToFileToIO = interpret $ \case
  WriteResult results -> embed $ toHtmlFile "test.html" $ plotTimeline results

plotTimeline :: [PipelineWithDuration] -> VegaLite
plotTimeline entries =
  let dates = map (toText . DT.formatDateTime "%Y-%m-%d-%H-%M" . Pipelines.createdAt) entries
      dat =
        dataFromColumns [Parse [("createdAt", FoUtc "%Y-%m-%d-%H-%M")]]
          . dataColumn "createdAt" (Strings dates) -- TODO: investigate hvega DateTime datatype
          . dataColumn "duration" (Strings (map (toText . duration) entries)) -- TODO: investigate hvega DateTime datatype
      enc =
        encoding
          . position X [PName "createdAt", PmType Temporal, PTimeUnit YearMonthDateHoursMinutesSeconds, PAxis [AxTitle "Date (Days)"]]
          . position Y [PName "duration", PmType Quantitative, PAxis [AxTitle "Pipeline duration"]]
          . row [FName "createdAt", FmType Temporal, FTimeUnit (Utc Year)]
   in toVegaLite
        [ dat [],
          mark Point [MFilled True, MTooltip TTEncoding],
          enc [],
          width 1600,
          height 400
        ]

projectsApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (ProjectsApi ': r) a -> Sem r a
projectsApiToIO baseUrl apiToken = interpret $ \case
  GetProjects groupId -> do
    let template = [uriTemplate|/api/v4/groups/{groupId}/projects?include_subgroups=true&archived=false|]
    embed $ fetchDataPaginated apiToken baseUrl template [("groupId", (stringValue . show) groupId)]
  GetProject project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}|]
    embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) project)]

mergeRequestApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (MergeRequestApi ': r) a -> Sem r a
mergeRequestApiToIO baseUrl apiToken = interpret $ \case
  GetOpenMergeRequests project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/merge_requests?state=opened|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) project)]
  EnableSourceBranchDeletionAfterMrMerge project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}?remove_source_branch_after_merge=true|]
    embed $ void <$> fetchDataX @Project baseUrl apiToken (setRequestMethod "PUT") template [("projectId", (stringValue . show) project)]

branchesApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (BranchesApi ': r) a -> Sem r a
branchesApiToIO baseUrl apiToken = interpret $ \case
  GetBranches project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/repository/branches|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) project)]

pipelinesApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (PipelinesApi ': r) a -> Sem r a
pipelinesApiToIO baseUrl apiToken = interpret $ \case
  GetPipeline project pipeline -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
    embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)]
  GetSuccessfulPipelines pId ref -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref=master&status={status}&updated_after=2019-06-09T08:00:00Z|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) pId), ("ref", (stringValue . show) ref), ("status", stringValue "success")]

schedulesApiToIO :: Member (Embed IO) r => BaseUrl -> ApiToken -> Sem (SchedulesApi ': r) a -> Sem r a
schedulesApiToIO baseUrl apiToken = interpret $ \case
  GetSchedules project -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipeline_schedules|]
    embed $ fetchDataPaginated apiToken baseUrl template [("projectId", (stringValue . show) project)]

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

fetchDataX :: (FromJSON a) => BaseUrl -> ApiToken -> (Request -> Request) -> Template -> [(String, Value)] -> IO (Either UpdateError a)
fetchDataX baseUrl apiToken reqTransformer template vars = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    Left invalidUrl -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchData' (reqTransformer . setTimeout . addToken apiToken $request)

bar :: BaseUrl -> ApiToken -> (Request -> Request) -> Template -> [(String, Value)] -> Either UpdateError Request
bar baseUrl apiToken reqTransformer template vars =
  bimap
    ExceptionError
    (reqTransformer . setTimeout . addToken apiToken)
    (parseRequest (show baseUrl <> "/" <> expand vars template))

fetchData :: (FromJSON a) => BaseUrl -> ApiToken -> Template -> [(String, Value)] -> IO (Either UpdateError a)
fetchData baseUrl apiToken = fetchDataX baseUrl apiToken id

fetchData' :: (FromJSON a) => Request -> IO (Either UpdateError a)
fetchData' request = do
  result <- try (mapLeft ConversionError . getResponseBody <$> httpJSONEither request)
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

setTimeout :: RequestTransformer
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

addToken :: ApiToken -> RequestTransformer
addToken apiToken = setRequestHeader "PRIVATE-TOKEN" [encodeUtf8 (coerce apiToken :: Text)]

parseNextRequest :: Response a -> Maybe Request
parseNextRequest response = parseNextHeader response >>= rightToMaybe . requestFromURI

parseNextHeader :: Response a -> Maybe URI
parseNextHeader response = href <$> find isNextLink (getResponseHeader "link" response >>= concat . parseLinkHeaderBS)

isNextLink :: Link -> Bool
isNextLink (Link _ [(Rel, "next")]) = True
isNextLink _ = False

removeApiTokenFromUpdateError :: UpdateError -> UpdateError
removeApiTokenFromUpdateError (HttpError httpException) = HttpError (removeApiTokenFromHttpException httpException)
removeApiTokenFromUpdateError (ConversionError jsonException) = ConversionError (removeApiTokenFromJsonException jsonException)
removeApiTokenFromUpdateError (ExceptionError x) = ExceptionError x

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

removeApiTokenFromRequest :: RequestTransformer
removeApiTokenFromRequest = set (headers . tokenHeader) "xxxxx"
