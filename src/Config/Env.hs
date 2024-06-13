module Config.Env (parseFromEnv) where

import Config.Types (ApiToken (ApiToken), BaseUrl (BaseUrl), GroupId (GroupId), PartialConfig (PartialConfig))
import qualified Data.Semigroup as S (First (..))
import qualified Env as E
import Network.URI (URI, parseAbsoluteURI)
import Relude hiding (Reader)
import System.Environment (getEnvironment)

parseFromEnv :: IO (PartialConfig Maybe)
parseFromEnv = do
  environment <- getEnvironment
  let partialConfig = E.parsePure partialConfigParser environment
  pure $ fromRight mempty partialConfig

partialConfigParser :: E.Parser E.Error (PartialConfig Maybe)
partialConfigParser =
  PartialConfig
    <$> maybeFirstParser (GroupId <$> E.var E.auto "HB_GROUP_ID" (E.help "ID of the Gitlab group for which the hooks should be set"))
    <*> maybeFirstParser (BaseUrl <$> E.var (absoluteURIFromEnv <=< E.nonempty) "HB_BASE_URL" (E.help "Base URL of the Gitlab instance (e.g. `https://gitlab.com/`)"))
    <*> maybeFirstParser (ApiToken <$> E.var (E.str <=< E.nonempty) "HB_API_TOKEN" (E.help "API Token to use for authorizing requests against the Gitlab API. `api` scope is required."))

maybeFirstParser :: E.Parser E.Error a -> E.Parser E.Error (Maybe (S.First a))
maybeFirstParser parser = fmap S.First <$> optional parser

absoluteURIFromEnv :: E.Reader E.Error URI
absoluteURIFromEnv s = maybeToRight (E.UnreadError "") (parseAbsoluteURI s)
