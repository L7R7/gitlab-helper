module Config.Optparse (parseConfigFromOptions) where

import Config.Types (ApiToken (ApiToken), BaseUrl (BaseUrl), GroupId (GroupId), PartialConfig (PartialConfig))
import qualified Config.Types
import qualified Data.Semigroup as S (First (..))
import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative
import Relude

parseConfigFromOptions :: IO (PartialConfig Maybe)
parseConfigFromOptions =
  execParser
    $ info
      (helper <*> parser)
      ( fullDesc
          <> progDesc "gitlab-helper"
          <> header "gitlab-helper - a collection of utilities for dealing with a load of projects in Gitlab"
          <> footer "For the commands that are not read-only, use \"-x\" to make them actually do stuff"
      )

parser :: Parser (PartialConfig Maybe)
parser =
  PartialConfig
    <$> optionalParser (option (GroupId <$> auto) (long "group-id" <> help "set the ID of the group to look at" <> metavar "ID"))
    <*> optionalParser (option (BaseUrl <$> eitherReader f) (long "base-url" <> help "Base URL of the Gitlab instance (e.g. `https://gitlab.com/`)" <> metavar "URL"))
    <*> optionalParser (option (ApiToken <$> auto) (long "api-token" <> help "API Token to use for authorizing requests against the Gitlab API. `api` scope is required." <> metavar "TOKEN"))
    <*> (Just . S.First <$> Config.Types.parser)
  where
    f :: String -> Either String URI
    f s = maybeToRight ("\"" <> s <> "\" is not a valid absolute URI") (parseAbsoluteURI s)
    optionalParser p = fmap S.First <$> optional p
