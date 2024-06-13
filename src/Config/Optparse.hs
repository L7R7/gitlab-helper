module Config.Optparse (parseConfigFromOptions) where

import Barbies (TraversableB (btraverse))
import Config.Types
import Data.List.Split
import qualified Data.Semigroup as S (First (..))
import Gitlab.Client (ApiToken (..), BaseUrl (..))
import Gitlab.Lib (Id (..))
import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative
import Relude

parseConfigFromOptions :: IO (PartialConfig (Compose Maybe S.First))
parseConfigFromOptions =
  execParser
    $ info
      (helper <*> parser)
      ( fullDesc
          <> progDesc "gitlab-helper"
          <> header "gitlab-helper - a collection of utilities for dealing with a load of projects in Gitlab"
          <> footer "For the commands that are not read-only, use \"-x\" to make them actually do stuff"
      )

parser :: Parser (PartialConfig (Compose Maybe S.First))
parser =
  btraverse (fmap Compose . optional . fmap S.First)
    $ PartialConfig
      (option (Id <$> auto) (long "group-id" <> help "set the ID of the group to look at" <> metavar "ID"))
      (option (BaseUrl <$> eitherReader f) (long "base-url" <> help "Base URL of the Gitlab instance (e.g. `https://gitlab.com/`)" <> metavar "URL"))
      (option (ApiToken <$> str) (long "api-token" <> help "API Token to use for authorizing requests against the Gitlab API. `api` scope is required." <> metavar "TOKEN"))
      (option (fmap Id <$> eitherReader g) (long "exclude-projects" <> help "set the list of projects to exclude as a comma-separated list of IDs" <> metavar "ID1,ID2,ID3"))
      commandParser
  where
    f :: String -> Either String URI
    f s = maybeToRight ("\"" <> s <> "\" is not a valid absolute URI") (parseAbsoluteURI s)
    g :: String -> Either String [Int]
    g "" = Right []
    g s = traverse (\s' -> maybeToRight ("\"" <> s' <> "\" is not a valid Project ID") $ readMaybe s') $ splitOn "," s

commandParser :: Parser Command
commandParser =
  hsubparser
    $ mconcat
      [ command "version" (info (pure Version) (progDesc "print program version")),
        command "show-branches" (info (pure ShowBranches) (progDesc "show branches")),
        command "show-projects" (info (pure ShowProjects) (progDesc "show projects")),
        command "list-all-projects-meta" (info (pure ListAllProjectsMeta) (progDesc "list all the projects for all groups that are visible for the provided API token in (almost) meta compatible JSON format")),
        command "list-projects-meta" (info (pure ListProjectsMeta) (progDesc "list the projects for the given group in (almost) meta compatible JSON format")),
        command "enable-source-branch-deletion" (info (EnableSourceBranchDeletionAfterMerge <$> executionParser) (progDesc "enable source branch deletion after merge for all projects")),
        command "enable-all-discussions-must-be-resolved-for-merge-requirement" (info (EnableAllDiscussionsMustBeResolvedForMergeRequirement <$> executionParser) (progDesc "enable the requirement that all discussions must be resolved for an MR to be merged for all projects")),
        command "enable-successful-pipeline-for-merge-requirement" (info (EnableSuccessfulPipelineForMergeRequirement <$> executionParser) (progDesc "enable the requirement that there must be a successful pipeline for an MR to be merged for all projects. CAUTION: Use with care, might not do what you want in projects without pipelines")),
        command "show-schedules" (info (pure ShowSchedules) (progDesc "show schedules")),
        command "show-merge-requests" (info (pure ShowMergeRequests) (progDesc "show projects with and without enabled merge requests, list merge requests")),
        command "count-deployments" (info (CountSuccessfulDeployments <$> argument (Year <$> auto) (metavar "YEAR")) (progDesc "count the number of successful deployments per project (a successful push pipeline on the master branch is counted as a deployment)")),
        command "set-merge-method-to-fast-forward" (info (SetMergeMethodToFastForward <$> executionParser) (progDesc "Set the merge method for all projects to \"Fast Forward\"")),
        command "update-merge-requests" (info mergeRequestUpdatActionParser (progDesc "Update all MRs from a given user that match a given condition with a given command"))
      ]

mergeRequestUpdatActionParser :: Parser Command
mergeRequestUpdatActionParser =
  UpdateMergeRequests
    <$> argument (eitherReader f) (metavar "ACTION" <> help "The action to perform. Must be one of \"rebase\", \"merge\", \"draft\", \"ready\"")
    <*> option (AuthorIs <$> auto) (short 'u' <> long "user-id" <> help "only MRs opened by the user with this ID are taken into account" <> metavar "ID")
    <*> optional
      ( (Left . SearchTerm <$> strOption (short 's' <> long "search" <> help "Optional. a string that must appear in the MR description or title. Mutually exclusive with --search-title" <> metavar "TXT"))
          <|> Right
          . SearchTermTitle
          <$> strOption (long "search-title" <> help "Optional. a string that must appear in the MR title. Mutually exclusive with --search" <> metavar "TXT")
      )
    <*> executionParser
  where
    f :: String -> Either String MergeRequestUpdateAction
    f "rebase" = Right Rebase
    f "merge" = Right Merge
    f "draft" = Right SetToDraft
    f "ready" = Right MarkAsReady
    f s = Left $ s <> " is not a valid MergeRequestUpdateAction"

executionParser :: Parser Execution
executionParser =
  (\b -> if b then Execute else DryRun) <$> switch (long "execute" <> short 'x' <> help "whether to actually change the world via the API. By default, only a dry run will be performed")
