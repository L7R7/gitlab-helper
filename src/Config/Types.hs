{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.Types
  ( Config (..),
    GroupId (..),
    BaseUrl (..),
    ApiToken (..),
    PartialConfig (..),
  )
where

import Data.Aeson (FromJSON (..))
import qualified Data.Semigroup as S (Last (..))
import Network.URI (URI)
import Relude hiding (Reader)

data Config = Config
  { groupId :: GroupId,
    baseUrl :: BaseUrl,
    apiToken :: ApiToken
  }
  deriving (Show)

data PartialConfig = PartialConfig
  { pGroupId :: Maybe (S.Last GroupId),
    pBaseUrl :: Maybe (S.Last BaseUrl),
    pApiToken :: Maybe (S.Last ApiToken)
  }

instance Semigroup PartialConfig where
  PartialConfig groupId1 baseUrl1 apiToken1 <> PartialConfig groupId2 baseUrl2 apiToken2 =
    PartialConfig (groupId1 <> groupId2) (baseUrl1 <> baseUrl2) (apiToken1 <> apiToken2)

instance Monoid PartialConfig where
  mempty = PartialConfig mempty mempty mempty

newtype GroupId = GroupId Int deriving newtype (FromJSON, Show)

newtype BaseUrl = BaseUrl URI deriving newtype (Show)

newtype ApiToken = ApiToken Text deriving newtype (FromJSON, Show)
