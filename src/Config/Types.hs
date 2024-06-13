{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Config.Types
  ( Config (..),
    GroupId (..),
    BaseUrl (..),
    ApiToken (..),
    PartialConfig (..),
    Command (..),
    MergeRequestUpdateAction (..),
    AuthorIs (..),
    MergeRequestTitleFilter (..),
    MergeRequestDescriptionFilter (..),
    Execution (..),
    commandParser,
  )
where

import Autodocodec
import Barbies
import Data.Aeson (FromJSON (..))
import qualified Data.Semigroup as S (First (..))
import Network.URI (URI)
import Options.Applicative
import Relude hiding (Reader)

data Config = Config
  { groupId :: GroupId,
    baseUrl :: BaseUrl,
    apiToken :: ApiToken
  }
  deriving stock (Show)

data PartialConfig f = PartialConfig
  { pGroupId :: f (S.First GroupId),
    pBaseUrl :: f (S.First BaseUrl),
    pApiToken :: f (S.First ApiToken)
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
  | CountSuccessfulDeploymentsIn2022
  | UpdateMergeRequests MergeRequestUpdateAction AuthorIs (Maybe MergeRequestTitleFilter) (Maybe MergeRequestDescriptionFilter) Execution
  deriving stock (Show)

data Execution = DryRun | Execute deriving stock (Eq, Show)

data MergeRequestUpdateAction = Rebase | Merge deriving stock (Show)

newtype AuthorIs = AuthorIs Int deriving stock (Show)

newtype MergeRequestTitleFilter = TitleContains String deriving stock (Show)

newtype MergeRequestDescriptionFilter = DescriptionContains String deriving stock (Show)

commandParser :: IO Command
commandParser =
  execParser
    $ info
      (helper <*> parser)
      ( fullDesc
          <> progDesc "gitlab-helper"
          <> header "gitlab-helper - a collection of utilities for dealing with a load of projects in Gitlab"
          <> footer "For the commands that are not read-only, use \"-x\" to make them actually do stuff"
      )

parser :: Parser Command
parser =
  hsubparser
    $ mconcat
      [ command "show-branches" (info (pure ShowBranches) (progDesc "show branches")),
        command "show-projects" (info (pure ShowProjects) (progDesc "show projects")),
        command "list-all-projects-meta" (info (pure ListAllProjectsMeta) (progDesc "list all the projects for all groups that are visible for the provided API token in (almost) meta compatible JSON format")),
        command "list-projects-meta" (info (pure ListProjectsMeta) (progDesc "list the projects for the given group in (almost) meta compatible JSON format")),
        command "enable-source-branch-deletion" (info (EnableSourceBranchDeletionAfterMerge <$> executionParser) (progDesc "enable source branch deletion after merge for all projects")),
        command "enable-all-discussions-must-be-resolved-for-merge-requirement" (info (EnableAllDiscussionsMustBeResolvedForMergeRequirement <$> executionParser) (progDesc "enable the requirement that all discussions must be resolved for an MR to be merged for all projects")),
        command "enable-successful-pipeline-for-merge-requirement" (info (EnableSuccessfulPipelineForMergeRequirement <$> executionParser) (progDesc "enable the requirement that there must be a successful pipeline for an MR to be merged for all projects. CAUTION: Use with care, might not do what you want in projects without pipelines")),
        command "show-schedules" (info (pure ShowSchedules) (progDesc "show schedules")),
        command "show-merge-requests" (info (pure ShowMergeRequests) (progDesc "show projects with and without enabled merge requests, list merge requests")),
        command "count-deployments" (info (pure CountSuccessfulDeploymentsIn2022) (progDesc "count the number of successful deployments per project (a successful push pipeline on the master branch is counted as a deployment)")),
        command "set-merge-method-to-fast-forward" (info (SetMergeMethodToFastForward <$> executionParser) (progDesc "Set the merge method for all projects to \"Fast Forward\"")),
        command "update-merge-requests" (info mergeRequestUpdatActionParser (progDesc "Update all MRs from a given user that match a given condition with a given command"))
      ]

mergeRequestUpdatActionParser :: Parser Command
mergeRequestUpdatActionParser =
  UpdateMergeRequests
    <$> argument (eitherReader f) (metavar "ACTION" <> help "The action to perform. Must be one of \"rebase\", \"merge\"")
    <*> option (AuthorIs <$> auto) (short 'u' <> long "user-id" <> help "only MRs opened by the user with this ID are taken into account" <> metavar "ID")
    <*> optional (TitleContains <$> strOption (short 't' <> long "title-contains" <> help "Optional. a string that must appear in the MR title" <> metavar "TXT"))
    <*> optional (DescriptionContains <$> strOption (short 'd' <> long "description-contains" <> help "Optional. a string that must appear in the MR description" <> metavar "TXT"))
    <*> executionParser
  where
    f :: String -> Either String MergeRequestUpdateAction
    f "rebase" = Right Rebase
    f "merge" = Right Merge
    f s = Left $ s <> " is not a valid MergeRequestUpdateAction"

executionParser :: Parser Execution
executionParser =
  ( \case
      False -> DryRun
      True -> Execute
  )
    <$> switch (long "execute" <> short 'x' <> help "whether to actually change the config via the API. By default, only a dry run will be performed")
