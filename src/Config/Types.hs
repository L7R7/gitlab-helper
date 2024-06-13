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
    Execution (..),
    commandParser,
  )
where

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
  deriving (Show)

data PartialConfig f = PartialConfig
  { pGroupId :: f (S.First GroupId),
    pBaseUrl :: f (S.First BaseUrl),
    pApiToken :: f (S.First ApiToken)
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

instance (Alternative f) => Semigroup (PartialConfig f) where
  PartialConfig groupId1 baseUrl1 apiToken1 <> PartialConfig groupId2 baseUrl2 apiToken2 =
    PartialConfig (groupId1 <|> groupId2) (baseUrl1 <|> baseUrl2) (apiToken1 <|> apiToken2)

instance (Alternative f) => Monoid (PartialConfig f) where
  mempty = bpure empty

newtype GroupId = GroupId Int deriving newtype (FromJSON, Show)

newtype BaseUrl = BaseUrl URI deriving newtype (Show)

newtype ApiToken = ApiToken Text deriving newtype (FromJSON, Show)

data Command
  = ShowBranches
  | EnableSourceBranchDeletionAfterMerge Execution
  | ShowProjects
  | ShowSchedules
  | ShowMergeRequests
  | EnableAllDiscussionsMustBeResolvedForMergeRequirement Execution
  | EnableSuccessfulPipelineForMergeRequirement Execution

data Execution = DryRun | Execute

commandParser :: IO Command
commandParser =
  execParser $
    info
      (helper <*> parser)
      (fullDesc <> progDesc "gitlab-helper" <> header "gitlab-helper - a collection of utilities for dealing with a load of projects in Gitlab")

parser :: Parser Command
parser =
  hsubparser $
    mconcat
      [ command "show-branches" (info (pure ShowBranches) (progDesc "show branches")),
        command "show-projects" (info (pure ShowProjects) (progDesc "show projects")),
        command "enable-source-branch-deletion" (info (EnableSourceBranchDeletionAfterMerge <$> executionParser) (progDesc "enable source branch deletion after merge for all projects")),
        command "enable-all-discussions-must-be-resolved-for-merge-requirement" (info (EnableAllDiscussionsMustBeResolvedForMergeRequirement <$> executionParser) (progDesc "enable the requirement that all discussions must be resolved for an MR to be merged for all projects")),
        command "enable-successful-pipeline-for-merge-requirement" (info (EnableSuccessfulPipelineForMergeRequirement <$> executionParser) (progDesc "enable the requirement that there must be a successful pipeline for an MR to be merged for all projects. CAUTION: Use with care, might not do what you want in projects without pipelines")),
        command "show-schedules" (info (pure ShowSchedules) (progDesc "show schedules")),
        command "show-merge-requests" (info (pure ShowMergeRequests) (progDesc "show projects with and without enabled merge requests, list merge requests"))
      ]

executionParser :: Parser Execution
executionParser =
  ( \case
      False -> DryRun
      True -> Execute
  )
    <$> switch (long "execute" <> short 'x' <> help "whether to actually change the config via the API. By default, only a dry run will be performed")
