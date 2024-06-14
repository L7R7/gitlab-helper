module GitlabUtils
  ( fetchData,
    fetchData',
    fetchDataPaginated,
    headRequest,
    GC.UpdateError (..),
  )
where

import Burrito
import Config.Types
import Data.Aeson (FromJSON)
import qualified Gitlab.Client as GC
import Network.HTTP.Simple
import Network.HTTP.Types (Status)
import Relude

fetchData :: (FromJSON a) => Template -> [(String, Value)] -> ReaderT Config IO (Either GC.UpdateError a)
fetchData template vars = do
  token <- asks apiToken
  host <- asks baseUrl
  liftIO $ GC.fetchData host token template vars

fetchData' :: (FromJSON a) => (Request -> Request) -> Template -> [(String, Value)] -> ReaderT Config IO (Either GC.UpdateError a)
fetchData' reqTransformer template vars = do
  token <- asks apiToken
  host <- asks baseUrl
  liftIO $ GC.fetchData' host token reqTransformer template vars

fetchDataPaginated :: (FromJSON a) => Template -> [(String, Value)] -> ReaderT Config IO (Either GC.UpdateError [a])
fetchDataPaginated template vars = do
  token <- asks apiToken
  host <- asks baseUrl
  liftIO $ GC.fetchDataPaginated token host template vars

headRequest :: (Request -> Request) -> Template -> [(String, Value)] -> ReaderT Config IO (Either GC.UpdateError Status)
headRequest reqTransformer template vars = do
  token <- asks apiToken
  host <- asks baseUrl
  liftIO $ GC.headRequest host token reqTransformer template vars
