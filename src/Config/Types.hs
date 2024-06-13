{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Types
  ( Config (..),
    PartialConfig (..),
    Command (..),
    MergeRequestUpdateAction (..),
    AuthorIs (..),
    SearchTerm (..),
    SearchTermTitle (..),
    Year (..),
    Execution (..),
    partialConfigToConfig,
  )
where

import Barbies
import Gitlab.Client (ApiToken, BaseUrl)
import Gitlab.Group (Group)
import Gitlab.Lib (Id (..))
import Gitlab.Project (Project)
import Options.Applicative
import Relude hiding (Reader)

data Config = Config
  { groupId :: Id Group,
    baseUrl :: BaseUrl,
    apiToken :: ApiToken,
    projectsExcludeList :: [Id Project],
    cmd :: Command
  }
  deriving stock (Show)

data PartialConfig f = PartialConfig
  { pGroupId :: f (Id Group),
    pBaseUrl :: f BaseUrl,
    pApiToken :: f ApiToken,
    pProjectsExcludeList :: f [Id Project],
    pCommand :: f Command
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB, ApplicativeB, ConstraintsB)

instance (Alternative f) => Semigroup (PartialConfig f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (PartialConfig f) where
  mempty = bpure empty

data Command
  = Version
  | ShowBranches
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
  | UpdateMergeRequests MergeRequestUpdateAction AuthorIs (Maybe (Either SearchTerm SearchTermTitle)) Execution
  deriving stock (Show)

data Execution = DryRun | Execute deriving stock (Eq, Show)

data MergeRequestUpdateAction = List | Rebase | Merge | SetToDraft | MarkAsReady deriving stock (Bounded, Enum, Show)

newtype AuthorIs = AuthorIs Int deriving stock (Show)

newtype SearchTerm = SearchTerm String deriving stock (Show)

newtype SearchTermTitle = SearchTermTitle String deriving stock (Show)

newtype Year = Year Int deriving stock (Show)

partialConfigToConfig :: PartialConfig Identity -> Config
partialConfigToConfig (PartialConfig (Identity groupId) (Identity baseUrl) (Identity apiToken) (Identity projectsExcludeList) (Identity cmd)) = Config {..}
