{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Projects
  ( showProjectsForGroup,
    enableSourceBranchDeletionAfterMerge,
    enableSuccessfulPipelineForMergeRequirement,
    enableAllDiscussionsResolvedForMergeRequirement,
  )
where

import Colonnade
import Colourista (bold, formatWith)
import Config.Types
import qualified Data.Map as M
import Data.Text (toLower)
import Effects
import Polysemy
import Relude hiding (pi)

showProjectsForGroup :: (Member ProjectsApi r, Member Writer r) => GroupId -> Sem r ()
showProjectsForGroup gId = do
  write "=================================================="
  write $ "Listing the projects for Group " <> show gId
  getProjects gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      write . toText $ tableReport (sortOn (toLower . getProjectName . name) projects)
      writeSummary $ foldMap summarizeSingle projects

tableReport :: Foldable f => f Project -> String
tableReport =
  asciiCapped $
    mconcat
      [ cap
          ""
          (headed "ID" (show . projectId) <> headed "Name" (show . name) <> headed "default branch" (maybe "-" (\(Ref r) -> toString r) . defaultBranch)),
        cap
          "Merge Requests"
          (headed "enabled" (show . mergeRequestsEnabled) <> headed "Remove source branch after merge" (maybe "unknown" show . removeSourceBranchAfterMerge) <> headed "merge method" (show . mergeMethod)),
        cap
          "Requirements for a merge request to be merged"
          (headed "successful pipeline" (maybe "unknown" show . onlyAllowMergeIfPipelineSucceeds) <> headed "all discussions resolved" (maybe "unknown" show . onlyAllowMergeIfAllDiscussionsAreResolved)),
        cap
          "Pipelines"
          (headed "auto cancel pending" (show . autoCancelPendingPipelines))
      ]

data Processor r = Processor
  { groupId :: GroupId,
    title :: GroupId -> Text,
    predicate :: Project -> Bool,
    action :: ProjectId -> Sem r (Either UpdateError ())
  }

enableSourceBranchDeletionAfterMerge :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => Execution -> GroupId -> Sem r ()
enableSourceBranchDeletionAfterMerge execution gId =
  runProcessor $
    Processor
      gId
      (\gi -> "Enabling automatic branch deletion after MR merge for Group " <> show gi)
      (\p -> Just True == removeSourceBranchAfterMerge p)
      ( \pi -> case execution of
          DryRun -> Right () <$ write ("Dry Run. Pretending to set option for Project " <> show pi)
          Execute -> enableSourceBranchDeletionAfterMrMerge pi
      )

enableSuccessfulPipelineForMergeRequirement :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => Execution -> GroupId -> Sem r ()
enableSuccessfulPipelineForMergeRequirement execution gId =
  runProcessor $
    Processor
      gId
      (\gi -> "Enabling the requirement that a successful pipeline is required for a MR to be merged for Group " <> show gi)
      (or . onlyAllowMergeIfPipelineSucceeds)
      (\pId -> getProject pId >>= (projectHasCi >=> configureOption execution pId))

projectHasCi :: (Member ProjectsApi r) => Either UpdateError Project -> Sem r (Either UpdateError Bool)
projectHasCi (Left err) = pure $ Left err
projectHasCi (Right (Project pId _ _ _ (Just ref) _ _ _ _)) = hasCi pId ref
projectHasCi (Right _) = pure $ Right False -- no default branch, no CI

configureOption :: (Member MergeRequestApi r, Member Writer r) => Execution -> ProjectId -> Either UpdateError Bool -> Sem r (Either UpdateError ())
configureOption _ _ (Left err) = pure $ Left err
configureOption DryRun _ (Right False) = Right () <$ write "Dry Run. Pretending to unset option for project" >> logUnset
configureOption Execute pId (Right False) = unsetSuccessfulPipelineRequirementForMerge pId >> logUnset
configureOption DryRun _ (Right True) = Right () <$ write "Dry Run. Pretending to set option for project"
configureOption Execute pId (Right True) = setSuccessfulPipelineRequirementForMerge pId

logUnset :: Member Writer r => Sem r (Either UpdateError ())
logUnset = write "Project doesn't have CI. Deactivated the option." $> Right ()

enableAllDiscussionsResolvedForMergeRequirement :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => Execution -> GroupId -> Sem r ()
enableAllDiscussionsResolvedForMergeRequirement execution gId =
  runProcessor $
    Processor
      gId
      (\gi -> "Enabling the requirement that all discussions must be resolved for a MR to be merged for Group " <> show gi)
      (or . onlyAllowMergeIfAllDiscussionsAreResolved)
      ( case execution of
          DryRun -> (\pId -> Right () <$ write ("Dry Run. Pretending to set option for Project " <> show pId))
          Execute -> setResolvedDiscussionsRequirementForMerge
      )

runProcessor :: (Member ProjectsApi r, Member Writer r) => Processor r -> Sem r ()
runProcessor Processor {..} = do
  write "=================================================="
  write $ title groupId
  getProjects groupId >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse (process predicate action) projects
      write ""
      write "done: "
      let summary = foldl' (\m r -> M.insertWith (<>) r (Sum (1 :: Int)) m) (M.fromList $ (,mempty) <$> universe) res
      let summaryPrint = M.foldlWithKey' (\acc k (Sum c) -> (show k <> ": " <> show c) : acc) mempty summary
      traverse_ write summaryPrint

process :: (Member Writer r) => (Project -> Bool) -> (ProjectId -> Sem r (Either UpdateError ())) -> Project -> Sem r Result
process predicate action project = do
  write ""
  write $ formatWith [bold] ("=== " <> show (name project))
  if predicate project
    then write "option is already enabled. Not doing anything" $> AlreadySet
    else do
      write "setting option"
      res <- action (projectId project)
      case res of
        Left err -> write ("something went wrong. " <> show err) $> Error
        Right _ -> write "done" $> Set

data Result = AlreadySet | Set | Error deriving (Bounded, Enum, Eq, Ord, Show)

newtype Count a = Count Int
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

newtype EnabledDisabledCount a = EnabledDisabledCount (Count a, Count a)
  deriving newtype (Semigroup)
  deriving newtype (Monoid)

mkEnabledDisabledCount :: EnabledDisabled -> EnabledDisabledCount a
mkEnabledDisabledCount Enabled = EnabledDisabledCount (Count 1, Count 0)
mkEnabledDisabledCount Disabled = EnabledDisabledCount (Count 0, Count 1)

mkEnabledDisabledCount' :: Bool -> EnabledDisabledCount a
mkEnabledDisabledCount' b = if b then EnabledDisabledCount (Count 1, Count 0) else EnabledDisabledCount (Count 0, Count 1)

data ProjectCount

data SourceBranchDeletion

data HasNoDefaultBranch

data SuccessfulPipelineForMerge

data AllDiscussionsResolvedForMerge

data AutCancelRedundantPipelines

type Summary =
  ( Count ProjectCount,
    EnabledDisabledCount SourceBranchDeletion,
    Count HasNoDefaultBranch,
    EnabledDisabledCount SuccessfulPipelineForMerge,
    ( EnabledDisabledCount AllDiscussionsResolvedForMerge,
      EnabledDisabledCount AutCancelRedundantPipelines
    )
  )

writeSummary :: (Member Writer r) => Summary -> Sem r ()
writeSummary
  ( Count numProjects,
    EnabledDisabledCount (Count branchDeletionEnabled, Count branchDeletionDisabled),
    Count hasNoDefaultBranch,
    EnabledDisabledCount (Count successfulPipelineForMergeEnabled, Count successfulPipelineForMergeDisabled),
    ( EnabledDisabledCount (Count allDiscussionsResolvedForMergeEnabled, Count allDiscussionsResolvedForMergeDisabled),
      EnabledDisabledCount (Count autoCancelPendingPipelinesEnabled, Count autoCancelPendingPipelinesDisabled)
      )
    ) = do
    write $ formatWith [bold] "=== Summary"
    write $ "Anzahl Projekte: " <> show numProjects
    write $ "Projekte die 'remove_source_branch_after_merge' aktiviert haben: " <> show branchDeletionEnabled
    write $ "             'remove_source_branch_after_merge' NICHT aktiviert haben: " <> show branchDeletionDisabled
    write $ "Projekte die 'only_allow_merge_if_pipeline_succeeds' aktiviert haben: " <> show successfulPipelineForMergeEnabled
    write $ "             'only_allow_merge_if_pipeline_succeeds' NICHT aktiviert haben: " <> show successfulPipelineForMergeDisabled
    write $ "Projekte die 'only_allow_merge_if_all_discussions_are_resolved' aktiviert haben: " <> show allDiscussionsResolvedForMergeEnabled
    write $ "             'only_allow_merge_if_all_discussions_are_resolved' NICHT aktiviert haben: " <> show allDiscussionsResolvedForMergeDisabled
    write $ "Projekte die 'auto_cancel_pending_pipelines' aktiviert haben: " <> show autoCancelPendingPipelinesEnabled
    write $ "             'auto_cancel_pending_pipelines' NICHT aktiviert haben: " <> show autoCancelPendingPipelinesDisabled
    write $ "Projekte ohne default branch: " <> show hasNoDefaultBranch

summarizeSingle :: Project -> Summary
summarizeSingle project =
  ( Count 1,
    sourceBranchDeletionEnabledDisabled,
    noDefaultBranch,
    successfulPipelineForMergeEnabledDisabled,
    ( allDiscussionsResolvedForMergeEnabledDisabled,
      autoCancelPendingPipelinesEnabledDisabled
    )
  )
  where
    sourceBranchDeletionEnabledDisabled = mkEnabledDisabledCount' branchDeletionEnabled
    branchDeletionEnabled = or (removeSourceBranchAfterMerge project)
    noDefaultBranch = Count $ if isJust (defaultBranch project) then 0 else 1
    successfulPipelineForMergeEnabledDisabled = mkEnabledDisabledCount' $ or $ onlyAllowMergeIfPipelineSucceeds project
    allDiscussionsResolvedForMergeEnabledDisabled = mkEnabledDisabledCount' $ or $ onlyAllowMergeIfAllDiscussionsAreResolved project
    autoCancelPendingPipelinesEnabledDisabled = mkEnabledDisabledCount (autoCancelPendingPipelines project)
