module Config.Config (parseConfigOrDie) where

import Config.Env
import Config.File
import Config.Types
import Data.Either.Validation
import qualified Data.Semigroup as S
import Relude

parseConfigOrDie :: IO Config
parseConfigOrDie = parseConfig >>= either (die . show) pure

parseConfig :: IO (Either (NonEmpty String) Config)
parseConfig = do
  partialFromHome <- readPartialOptionsFromHomeDir
  partialFromLocal <- readPartialOptionsFromLocalDir
  partialFromEnv <- parseFromEnv
  pure $ mkConfig (partialFromHome <> partialFromLocal <> partialFromEnv)

mkConfig :: PartialConfig -> Either (NonEmpty String) Config
mkConfig (PartialConfig gId url token) =
  validationToEither $
    Config <$> x "Group ID missing" gId <*> x "Base URL missing" url <*> x "API-Token missing" token

x :: String -> Maybe (S.Last a) -> Validation (NonEmpty String) a
x err Nothing = Failure $ err :| []
x _ (Just (S.Last a)) = Success a
