{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipelines (showPipelineDurationsForProject, PipelineWithDuration (..), WriteToFile (..), writeResult, showPipelineWithDuration) where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Effects
import Network.URI (URI)
import Polysemy
import Polysemy.Internal (send)
import Relude hiding (id)
import System.Posix.Types (EpochTime)

data WriteToFile m a where
  WriteResult :: [PipelineWithDuration] -> WriteToFile m ()

writeResult :: (Member WriteToFile r) => [PipelineWithDuration] -> Sem r ()
writeResult x = send (WriteResult x :: WriteToFile (Sem r) ())

showPipelineDurationsForProject :: (Member WriteToFile r, Member PipelinesApi r, Member ProjectsApi r, Member Writer r) => ProjectId -> Sem r ()
showPipelineDurationsForProject pId = do
  write "starting..."
  project <- getProject pId
  case project of
    Left err -> write $ "something went wrong" <> show err
    Right (Project _ _ _ _ Nothing _ _ _ _ _ _) -> write $ "Found project without default branch: " <> show pId
    Right (Project _ _ _ _ (Just ref) _ _ _ _ _ _) -> do
      results <- evaluateProject pId ref
      writeResult $ filter (\(PipelineWithDuration _ _ _ _ _ _ so) -> so /= SourceSchedule) $ sortOn (\(PipelineWithDuration _ _ _ _ ut _ _) -> ut) results

evaluateProject :: (Member PipelinesApi r, Member Writer r) => ProjectId -> Ref -> Sem r [PipelineWithDuration]
evaluateProject pId ref = do
  compactPipelinesResult <- getSuccessfulPipelines pId ref
  pipelinesResult <- case compactPipelinesResult of
    Left err -> do
      write $ unwords ["Something went wrong getting the pipelines for project", show pId, ",", show err]
      pure []
    Right pipelines -> traverse (getPipeline pId . compactPipelineId) pipelines
  let (_, ps) = partitionEithers pipelinesResult
  pure (mapMaybe f ps)

f :: Pipeline -> Maybe PipelineWithDuration
f (Pipeline id sha (Just duration) (Just queuedDuration) createdAt webUrl source) = Just PipelineWithDuration {..}
f Pipeline {} = Nothing

data PipelineWithDuration = PipelineWithDuration
  { id :: PipelineId,
    sha :: Sha,
    duration :: Duration,
    queuedDuration :: Duration,
    createdAt :: UTCTime,
    webUrl :: URI,
    source :: Source
  }

showPipelineWithDuration :: PipelineWithDuration -> Text
showPipelineWithDuration (PipelineWithDuration _ _ duration queued created _ source) = (show . utcTimeToEpochTime) created <> ";" <> show duration <> "," <> show queued <> "," <> show source

utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = fromIntegral . toSecs
  where
    toSecs :: UTCTime -> Int
    toSecs = round . utcTimeToPOSIXSeconds
