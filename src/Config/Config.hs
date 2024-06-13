module Config.Config (parseConfigOrDie) where

import Barbies (bsequence', bzipWith)
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
  mkConfig <$> sequence [parseConfigFromOptions, parseFromEnv, readPartialOptionsFromLocalDir, readPartialOptionsFromHomeDir]

mkConfig :: [PartialConfig (Compose Maybe S.First)] -> Either (NonEmpty String) Config
mkConfig = mkConfig' . mconcat

mkConfig' :: PartialConfig (Compose Maybe S.First) -> Either (NonEmpty String) Config
mkConfig' = validationToEither . fmap partialConfigToConfig . bsequence' . bzipWith fieldValueOrMissing msgs
  where
    msgs = PartialConfig (Const "Group ID missing") (Const "Base URL missing") (Const "API-Token missing") (Const "Command misisng")
    fieldValueOrMissing :: Const String a -> Compose Maybe S.First a -> Validation (NonEmpty String) a
    fieldValueOrMissing (Const err) (Compose Nothing) = Failure $ err :| []
    fieldValueOrMissing _ (Compose (Just (S.First a))) = Success a
