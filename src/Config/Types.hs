{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Types
  ( Config (..),
    PartialConfig (..),
    Command (..),
    MergeRequestUpdateAction (..),
    MetaScope (..),
    MergeCiOption (..),
    AuthorIs (..),
    SearchTerm (..),
    SearchTermTitle (..),
    Year (..),
    WithArchivedProjects (..),
    MergeStatusRecheck (..),
    Execution (..),
    partialConfigToConfig,
  )
where

import Barbies
import Gitlab.Client.MTL (ApiToken, BaseUrl, UserAgent)
import Gitlab.Group (Group)
import Gitlab.Lib (Id (..))
import Gitlab.Project (Project)
import Options.Applicative
import Relude hiding (Reader)

data Config = Config
  { groupId :: Id Group,
    baseUrl :: BaseUrl,
    apiToken :: ApiToken,
    userAgent :: UserAgent,
    projectsExcludeList :: [Id Project],
    cmd :: Command
  }
  deriving stock (Show)

data PartialConfig f = PartialConfig
  { pGroupId :: f (Id Group),
    pBaseUrl :: f BaseUrl,
    pApiToken :: f ApiToken,
    pUserAgent :: f UserAgent,
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
  | ListProjectsMeta MetaScope
  | ShowSchedules
  | ShowMergeRequests MergeStatusRecheck
  | EnableAllDiscussionsMustBeResolvedForMergeRequirement Execution
  | EnableSuccessfulPipelineForMergeRequirement Execution
  | SetMergeMethodToFastForward Execution
  | CountSuccessfulDeployments Year WithArchivedProjects
  | UpdateMergeRequests MergeRequestUpdateAction (Maybe AuthorIs) (Maybe (Either SearchTerm SearchTermTitle)) MergeStatusRecheck Execution
  deriving stock (Show)

data Execution = DryRun | Execute deriving stock (Eq, Show)

data MetaScope = MetaScopeGroup | MetaScopeAll deriving stock (Eq, Show)

data MergeRequestUpdateAction = List | Rebase | Merge MergeCiOption | SetToDraft | MarkAsReady deriving stock (Show)

data MergeCiOption = PipelineMustSucceed | SkipCi deriving stock (Show)

newtype AuthorIs = AuthorIs Int deriving stock (Show)

newtype SearchTerm = SearchTerm String deriving stock (Show)

newtype SearchTermTitle = SearchTermTitle String deriving stock (Show)

newtype Year = Year Int deriving stock (Show)

data WithArchivedProjects = IncludeArchivedProjects | SkipArchivedProjects deriving stock (Show)

data MergeStatusRecheck = RecheckMergeStatus | NoRecheckMergeStatus deriving stock (Show)

partialConfigToConfig :: PartialConfig Identity -> Config
partialConfigToConfig (PartialConfig (Identity groupId) (Identity baseUrl) (Identity apiToken) (Identity userAgent) (Identity projectsExcludeList) (Identity cmd)) = Config {..}
