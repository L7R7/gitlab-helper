module Config.Env (parseFromEnv) where

import Barbies (TraversableB (btraverse))
import Config.Types (PartialConfig (PartialConfig))
import qualified Data.Semigroup as S (First (..))
import qualified Env as E
import Gitlab.Client.MTL (ApiToken (ApiToken), BaseUrl (BaseUrl))
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
      (Id <$> E.var E.auto "GLH_GROUP_ID" (E.help "ID of the Gitlab group you're working in"))
      (BaseUrl <$> E.var (absoluteURIFromEnv <=< E.nonempty) "GLH_BASE_URL" (E.help "Base URL of the Gitlab instance (e.g. `https://gitlab.com/`)"))
      (ApiToken <$> E.var (E.str <=< E.nonempty) "GLH_API_TOKEN" (E.help "API Token to use for authorizing requests against the Gitlab API. `api` scope is required."))
      (fmap Id <$> E.var (traverse E.auto <=< E.splitOn ',') "GLH_EXCLUDE_PROJECTS" (E.help "List of projects to exclude"))
      empty

maybeFirstParser :: E.Parser E.Error a -> E.Parser E.Error (Compose Maybe S.First a)
maybeFirstParser parser = Compose . fmap S.First <$> optional parser

absoluteURIFromEnv :: E.Reader E.Error URI
absoluteURIFromEnv s = maybeToRight (E.UnreadError "") (parseAbsoluteURI s)
