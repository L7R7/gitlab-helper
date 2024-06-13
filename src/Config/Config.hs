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
  pure $ mkConfig [partialFromEnv, partialFromLocal, partialFromHome]

mkConfig :: [PartialConfig Maybe] -> Either (NonEmpty String) Config
mkConfig = mkConfig' . mconcat

mkConfig' :: PartialConfig Maybe -> Either (NonEmpty String) Config
mkConfig' (PartialConfig gId url token) =
  validationToEither
    $ Config
    <$> x "Group ID missing" gId
    <*> x "Base URL missing" url
    <*> x "API-Token missing" token

x :: String -> Maybe (S.First a) -> Validation (NonEmpty String) a
x err Nothing = Failure $ err :| []
x _ (Just (S.First a)) = Success a
