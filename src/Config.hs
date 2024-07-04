{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Config
  ( Config (..),
    Command (..),
    MergeRequestUpdateAction (..),
    MergeCiOption (..),
    AuthorIs (..),
    SearchTerm (..),
    SearchTermTitle (..),
    Year (..),
    WithArchivedProjects (..),
    MergeStatusRecheck (..),
    Execution (..),
  )
where

import Autodocodec
import Gitlab.Client.MTL (ApiToken (..), BaseUrl (..))
import Gitlab.Group (Group)
import Gitlab.Lib (Id (..))
import Gitlab.Project (Project)
import Network.URI (parseURI)
import OptEnvConf hiding (Command)
import Path
import Path.IO
import Relude hiding (Reader, reader)

data Config = Config
  { groupId :: Id Group,
    baseUrl :: BaseUrl,
    apiToken :: ApiToken,
    projectsExcludeList :: [Id Project],
    cmd :: Command
  }
  deriving stock (Show)

homeDirConfig :: Parser FilePath
homeDirConfig = mapIO (\homeIO -> homeIO >>= \home -> toFilePath <$> resolveFile home ".gitlab-helper.yml") (pure getHomeDir)

localConfig :: Parser FilePath
localConfig = mapIO id (pure (toFilePath <$> resolveFile' ".gitlab-helper.yml"))

instance HasParser Config where
  settingsParser =
    withCombinedYamlConfigs ((\l h -> [l, h]) <$> localConfig <*> homeDirConfig)
      $ subConfig "config"
      $ subEnv_ "GLH"
      $ Config
      <$> setting
        [ help "ID of the GitLab group to work with",
          reader (Id <$> positiveIntReader),
          long "group-id",
          option,
          env "GROUP_ID",
          conf "groupId",
          metavar "GROUP_ID"
        ]
      <*> setting
        [ help "Base URL of your GitLab instance (e.g. `https://my.gitlab.com`)",
          reader (BaseUrl <$> maybeReader parseURI),
          long "base-url",
          option,
          env "BASE_URL",
          conf "baseUrl",
          metavar "URL"
        ]
      <*> setting
        [ help "API Token to use for authorizing requests against the Gitlab API. `api` scope is required.",
          reader (ApiToken <$> auto),
          long "api-token",
          option,
          env "API_TOKEN",
          conf "apiToken",
          metavar "TOKEN"
        ]
      <*> setting
        [ help "set the list of projects to exclude as a comma-separated list of IDs",
          reader (commaSeparatedList (Id <$> positiveIntReader)),
          long "exclude-projects",
          option,
          conf "exclude-projects",
          env "EXCLUDE_PROJECTS",
          metavar "ID1,ID2,ID3",
          value []
        ]
      <*> settingsParser

instance HasCodec BaseUrl where
  codec = BaseUrl <$> bimapCodec (maybeToRight "can't parse URI" . parseURI) show stringCodec

instance HasCodec ApiToken where
  codec = dimapCodec ApiToken (\(ApiToken txt) -> txt) textCodec

positiveIntReader :: Reader Int
positiveIntReader = maybeReader (find (> 0) <$> (readMaybe @Int))

data Command
  = ShowBranches
  | EnableSourceBranchDeletionAfterMerge Execution
  | ShowProjects
  | ListAllProjectsMeta
  | ListProjectsMeta
  | ShowSchedules
  | ShowMergeRequests MergeStatusRecheck
  | EnableAllDiscussionsMustBeResolvedForMergeRequirement Execution
  | EnableSuccessfulPipelineForMergeRequirement Execution
  | SetMergeMethodToFastForward Execution
  | CountSuccessfulDeployments Year WithArchivedProjects
  | UpdateMergeRequests MergeRequestUpdateAction (Maybe AuthorIs) (Maybe (Either SearchTerm SearchTermTitle)) MergeStatusRecheck Execution
  deriving stock (Show)

instance HasParser Command where
  settingsParser =
    commands
      [ command "show-branches" "Show branches" $ pure ShowBranches,
        command "show-projects" "Show projects" $ pure ShowProjects,
        command "list-all-projects-meta" "List all the projects for all groups that are visible for the provided API token in (almost) meta compatible JSON format" $ pure ListAllProjectsMeta,
        command "list-projects-meta" "List the projects for the given group in (almost) meta compatible JSON format" $ pure ListProjectsMeta,
        command "enable-source-branch-deletion" "Enable source branch deletion after merge for all projects in the group" $ EnableSourceBranchDeletionAfterMerge <$> settingsParser,
        command "enable-all-discussions-must-be-resolved-for-merge-requirement" "Enable the requirement that all discussions must be resolved for an MR to be merged for all projects" $ EnableAllDiscussionsMustBeResolvedForMergeRequirement <$> settingsParser,
        command "enable-successful-pipeline-for-merge-requirement" "Enable the requirement that there must be a successful pipeline for an MR to be merged for all projects. CAUTION: Use with care, might not do what you want in projects without pipelines" $ EnableSuccessfulPipelineForMergeRequirement <$> settingsParser,
        command "show-schedules" "Show Pipeline Schedules" $ pure ShowSchedules,
        command "show-merge-requests" "Show projects with and without enabled merge requests, list open merge requests" $ ShowMergeRequests <$> settingsParser,
        command "count-deployments" "Count the number of successful deployments per project (a successful push pipeline on the default branch is counted as a deployment)" $ CountSuccessfulDeployments <$> settingsParser <*> settingsParser,
        command "set-merge-method-to-fast-forward" "Set the merge method for all projects to \"Fast Forward\"" $ SetMergeMethodToFastForward <$> settingsParser,
        command "update-merge-requests" "Update all MRs from a given user that match a given condition with a given command" mergeRequestUpdateCommandParser
      ]

mergeRequestUpdateCommandParser :: Parser Command
mergeRequestUpdateCommandParser =
  UpdateMergeRequests
    <$> settingsParser
    <*> optional
      ( setting
          [ help "only MRs opened by the user with this ID are taken into account",
            reader (AuthorIs <$> positiveIntReader),
            option,
            short 'u',
            long "user-id",
            metavar "USER_ID"
          ]
      )
    <*> optional (Right <$> settingsParser <|> Left <$> settingsParser)
    <*> settingsParser
    <*> settingsParser

data Execution = DryRun | Execute deriving stock (Eq, Show)

instance HasParser Execution where
  settingsParser =
    setting
      [ help "whether to actually change the world via the API. By default, only a dry run will be performed",
        switch Execute,
        long "execute",
        short 'x',
        value DryRun
      ]

data MergeRequestUpdateAction = List | Rebase | Merge MergeCiOption | SetToDraft | MarkAsReady deriving stock (Show)

instance HasParser MergeRequestUpdateAction where
  settingsParser =
    commands
      [ command "rebase" "rebase the merge requests" $ pure Rebase,
        command "merge" "merge the merge requests" $ Merge <$> settingsParser,
        command "draft" "set the merge requests to `draft`" $ pure SetToDraft,
        command "ready" "mark the merge requests as ready" $ pure MarkAsReady,
        command "list" "list the merge requests" $ pure List
      ]

data MergeCiOption = PipelineMustSucceed | SkipCi deriving stock (Show)

instance HasParser MergeCiOption where
  settingsParser =
    setting
      [ help "don't enforce that a merge request requires a successful pipeline to be merged (also helpful for projects that don't have pipelines on non-default branches)",
        switch SkipCi,
        value PipelineMustSucceed,
        long "skip-ci"
      ]

newtype AuthorIs = AuthorIs Int deriving stock (Show)

newtype SearchTerm = SearchTerm String deriving stock (Show)

instance HasParser SearchTerm where
  settingsParser =
    setting
      [ help "A string that must appear in the MR description or title.",
        option,
        reader (SearchTerm <$> str),
        short 's',
        long "search",
        metavar "TXT"
      ]

newtype SearchTermTitle = SearchTermTitle String deriving stock (Show)

instance HasParser SearchTermTitle where
  settingsParser =
    setting
      [ help "A string that must appear in the MR title.",
        option,
        reader (SearchTermTitle <$> str),
        long "search-title",
        metavar "TXT"
      ]

newtype Year = Year Int deriving stock (Show)

instance HasParser Year where
  settingsParser = setting [help "Set the year", option, reader (Year <$> positiveIntReader), long "year", metavar "YEAR"]

data WithArchivedProjects = IncludeArchivedProjects | SkipArchivedProjects deriving stock (Show)

instance HasParser WithArchivedProjects where
  settingsParser = (\b -> if b then IncludeArchivedProjects else SkipArchivedProjects) <$> yesNoSwitch False [help "Include or skip archived projects", long "include-archived-projects"]

data MergeStatusRecheck = RecheckMergeStatus | NoRecheckMergeStatus deriving stock (Show)

instance HasParser MergeStatusRecheck where
  settingsParser =
    (\b -> if b then RecheckMergeStatus else NoRecheckMergeStatus)
      <$> yesNoSwitch
        False
        [ help "Trigger a recheck of the merge status of the merge requests. This is done on the Gitlab server and might have a performance impact so it's not done by default",
          long "recheck-merge-status"
        ]
