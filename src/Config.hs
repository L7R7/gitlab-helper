{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
  ( Config (..),
    GroupId (..),
    BaseUrl (..),
    ApiToken (..),
    parseConfigOrDie,
  )
where

import Data.ByteString (ByteString)
import Data.Either.Combinators (maybeToRight)
import Env
import Network.URI (URI, parseAbsoluteURI)

data Config = Config
  { groupId :: GroupId,
    baseUrl :: BaseUrl,
    apiToken :: ApiToken
  }

newtype GroupId = GroupId Int deriving newtype (Show)

newtype BaseUrl = BaseUrl URI deriving newtype (Show)

newtype ApiToken = ApiToken ByteString deriving newtype (Show)

parseConfigOrDie :: IO Config
parseConfigOrDie =
  parse
    (header headerText)
    ( Config <$> (GroupId <$> var auto "HB_GROUP_ID" (help "ID of the Gitlab group for which the hooks should be set"))
        <*> (BaseUrl <$> var (absoluteURIFromEnv <=< nonempty) "HB_BASE_URL" (help "Base URL of the Gitlab instance (e.g. `https://gitlab.com/`)"))
        <*> (ApiToken <$> var (str <=< nonempty) "HB_API_TOKEN" (help "API Token to use for authorizing requests against the Gitlab API. `api` scope is required."))
    )
  where
    headerText = "mr-bot. Get all open merge requests for the projects in a group"

absoluteURIFromEnv :: Reader Error URI
absoluteURIFromEnv s = maybeToRight (UnreadError "") (parseAbsoluteURI s)
