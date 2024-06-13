module Config.Env (parseFromEnv) where

import Barbies (TraversableB (btraverse))
import Config.Types (ApiToken (ApiToken), BaseUrl (BaseUrl), GroupId (GroupId), PartialConfig (PartialConfig))
import qualified Data.Semigroup as S (First (..))
import qualified Env as E
import Gitlab.Lib (Id (..))
import Network.URI (URI, parseAbsoluteURI)
import Relude hiding (Reader)
import System.Environment (getEnvironment)

parseFromEnv :: IO (PartialConfig (Compose Maybe S.First))
parseFromEnv = do
  environment <- getEnvironment
  let partialConfig = E.parsePure partialConfigParser environment
  pure $ fromRight mempty partialConfig

partialConfigParser :: E.Parser E.Error (PartialConfig (Compose Maybe S.First))
partialConfigParser =
  btraverse maybeFirstParser
    $ PartialConfig
      (GroupId <$> E.var E.auto "HB_GROUP_ID" (E.help "ID of the Gitlab group for which the hooks should be set"))
      (BaseUrl <$> E.var (absoluteURIFromEnv <=< E.nonempty) "HB_BASE_URL" (E.help "Base URL of the Gitlab instance (e.g. `https://gitlab.com/`)"))
      (ApiToken <$> E.var (E.str <=< E.nonempty) "HB_API_TOKEN" (E.help "API Token to use for authorizing requests against the Gitlab API. `api` scope is required."))
      (fmap Id <$> E.var (traverse E.auto <=< E.splitOn ',') "HB_EXCLUDE_PROJECTS" (E.help "List of projects to exclude"))
      empty

maybeFirstParser :: E.Parser E.Error a -> E.Parser E.Error (Compose Maybe S.First a)
maybeFirstParser parser = Compose . fmap S.First <$> optional parser

absoluteURIFromEnv :: E.Reader E.Error URI
absoluteURIFromEnv s = maybeToRight (E.UnreadError "") (parseAbsoluteURI s)
