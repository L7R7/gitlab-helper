{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Types
  ( Config (..),
    GroupId (..),
    ProjectId (..),
    BaseUrl (..),
    ApiToken (..),
    PartialConfig (..),
    Command (..),
    MergeRequestUpdateAction (..),
    AuthorIs (..),
    SearchTerm (..),
    Year (..),
    Execution (..),
    partialConfigToConfig,
  )
where

import Autodocodec
import Barbies
import Data.Aeson (FromJSON (..))
import Network.URI (URI)
import Options.Applicative
import Relude hiding (Reader)

data Config = Config
  { groupId :: GroupId,
    baseUrl :: BaseUrl,
    apiToken :: ApiToken,
    projectsExcludeList :: [ProjectId],
    cmd :: Command
  }
  deriving stock (Show)

data PartialConfig f = PartialConfig
  { pGroupId :: f GroupId,
    pBaseUrl :: f BaseUrl,
    pApiToken :: f ApiToken,
    pProjectsExcludeList :: f [ProjectId],
    pCommand :: f Command
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB, ApplicativeB, ConstraintsB)

instance (Alternative f) => Semigroup (PartialConfig f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (PartialConfig f) where
  mempty = bpure empty

newtype GroupId = GroupId Int
  deriving newtype (Show)
  deriving (FromJSON) via (Autodocodec GroupId)

instance HasCodec GroupId where
  codec = dimapCodec GroupId (\(GroupId i) -> i) codec

newtype ProjectId = ProjectId {getProjectId :: Int}
  deriving newtype (Eq, Ord, Show)
  deriving (FromJSON) via (Autodocodec ProjectId)

instance HasCodec ProjectId where
  codec = dimapCodec ProjectId getProjectId boundedIntegralCodec

newtype BaseUrl = BaseUrl URI deriving newtype (Show)

newtype ApiToken = ApiToken Text deriving newtype (FromJSON, Show)

data Command
  = ShowBranches
  | EnableSourceBranchDeletionAfterMerge Execution
  | ShowProjects
  | ListAllProjectsMeta
  | ListProjectsMeta
  | ShowSchedules
  | ShowMergeRequests
  | EnableAllDiscussionsMustBeResolvedForMergeRequirement Execution
  | EnableSuccessfulPipelineForMergeRequirement Execution
  | SetMergeMethodToFastForward Execution
  | CountSuccessfulDeployments Year
  | UpdateMergeRequests MergeRequestUpdateAction AuthorIs (Maybe SearchTerm) Execution
  deriving stock (Show)

data Execution = DryRun | Execute deriving stock (Eq, Show)

data MergeRequestUpdateAction = Rebase | Merge deriving stock (Show)

newtype AuthorIs = AuthorIs Int deriving stock (Show)

newtype SearchTerm = SearchTerm String deriving stock (Show)

newtype Year = Year Int deriving stock (Show)

partialConfigToConfig :: PartialConfig Identity -> Config
partialConfigToConfig (PartialConfig (Identity groupId) (Identity baseUrl) (Identity apiToken) (Identity projectsExcludeList) (Identity cmd)) = Config {..}
