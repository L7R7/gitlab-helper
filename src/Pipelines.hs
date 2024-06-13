{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Pipelines (showPipelineDurationsForProject) where

import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Effects
import Network.URI (URI)
import Polysemy
import System.Posix.Types (EpochTime)
import Prelude hiding (id)

showPipelineDurationsForProject :: (Member PipelinesApi r, Member ProjectsApi r, Member Writer r) => ProjectId -> Sem r ()
showPipelineDurationsForProject pId = do
  project <- getProject pId
  case project of
    Left err -> write $ "something went wrong" <> show err
    Right (Project _ _ _ Nothing) -> write $ "Found project without default branch: " <> show pId
    Right (Project _ _ _ (Just ref)) -> do
      results <- evaluateProject pId ref
      traverse_ (write . show) $ sortOn (negate . id) results

evaluateProject :: (Member PipelinesApi r, Member Writer r) => ProjectId -> Ref -> Sem r [PipelineWithDuration]
evaluateProject pId ref = do
  compactPipelinesResult <- getSuccessfulPipelines pId ref
  pipelinesResult <- case compactPipelinesResult of
    Left err -> do
      write $ unwords ["Something went wrong getting the pipelines for project", show pId, ",", show err]
      pure []
    Right pipelines -> traverse (getPipeline pId . compactPipelineId) pipelines
  let (_, ps) = partitionEithers pipelinesResult
  pure . catMaybes $ f <$> ps

f :: Pipeline -> Maybe PipelineWithDuration
f (Pipeline _ _ Nothing _ _) = Nothing
f (Pipeline id sha (Just duration) createdAt webUrl) = Just PipelineWithDuration {..}

data PipelineWithDuration = PipelineWithDuration
  { id :: PipelineId,
    sha :: Sha,
    duration :: Duration,
    createdAt :: UTCTime,
    webUrl :: URI
  }

instance Eq PipelineWithDuration where
  p == p' = id p == id p'

instance Ord PipelineWithDuration where
  p <= p' = id p <= id p'

instance Show PipelineWithDuration where
  show (PipelineWithDuration pid _ duration created _) = (show . utcTimeToEpochTime) created <> ";" <> show duration

utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = fromIntegral . toSecs
  where
    toSecs :: UTCTime -> Int
    toSecs = round . utcTimeToPOSIXSeconds
