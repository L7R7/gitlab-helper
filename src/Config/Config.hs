module Config.Config (parseConfigOrDie) where

import Config.Env
import Config.File
import Config.Optparse
import Config.Types
import Data.Either.Validation
import qualified Data.Semigroup as S
import Relude

parseConfigOrDie :: IO Config
parseConfigOrDie = parseConfig >>= either (die . show) pure

parseConfig :: IO (Either (NonEmpty String) Config)
parseConfig = do
  -- todo: wenn man hier ein kluges fold macht, könnte man short-circuiten
  --       und ggf gar nicht erst versuchen eine source zu lesen wenn man sie eh nicht braucht
  mkConfig <$> sequence [readPartialOptionsFromHomeDir, readPartialOptionsFromLocalDir, parseFromEnv, parseConfigFromOptions]

mkConfig :: [PartialConfig Maybe] -> Either (NonEmpty String) Config
mkConfig = mkConfig' . mconcat

mkConfig' :: PartialConfig Maybe -> Either (NonEmpty String) Config
mkConfig' (PartialConfig gId url token c) =
  validationToEither
    $ Config
    <$> fieldValueOrMissing "Group ID missing" gId
    <*> fieldValueOrMissing "Base URL missing" url
    <*> fieldValueOrMissing "API-Token missing" token
    <*> fieldValueOrMissing "Command missing" c
  where
    fieldValueOrMissing :: String -> Maybe (S.First a) -> Validation (NonEmpty String) a
    fieldValueOrMissing err Nothing = Failure $ err :| []
    fieldValueOrMissing _ (Just (S.First a)) = Success a
