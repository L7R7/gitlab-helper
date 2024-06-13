{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Projects
  ( showProjectsForGroup,
    enableSourceBranchDeletionAfterMerge,
    enableSuccessfulPipelineForMergeRequirement,
    enableAllDiscussionsResolvedForMergeRequirement,
  )
where

import Colourista (bold, formatWith)
import Config.Types
import qualified Data.Map as M
import Data.Text (toLower)
import Effects
import Polysemy
import Relude
import Relude.Extra (universe)

showProjectsForGroup :: (Member ProjectsApi r, Member Writer r) => GroupId -> Sem r ()
showProjectsForGroup gId = do
  write "=================================================="
  write $ "Listing the projects for Group " <> show gId
  getProjects gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      traverse_ (write . show) (sortOn (toLower . getProjectName . name) projects)
      writeSummary $ foldMap summarizeSingle projects

enableSourceBranchDeletionAfterMerge :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => GroupId -> Sem r ()
enableSourceBranchDeletionAfterMerge gId = do
  write "=================================================="
  write $ "Enabling automatic branch deletion after MR merge for Group " <> show gId
  getProjects gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse process projects
      write ""
      write "done: "
      let summary = foldl' (\m r -> M.insertWith (<>) r (Sum (1 :: Int)) m) (M.fromList $ (,mempty) <$> universe) res
      let summaryPrint = M.foldlWithKey' (\acc k (Sum c) -> (show k <> ": " <> show c) : acc) mempty summary
      traverse_ write summaryPrint
  where
    process :: (Member MergeRequestApi r, Member Writer r) => Project -> Sem r Result
    process project = do
      write ""
      write $ formatWith [bold] ("=== " <> show (name project))
      if Just True == removeSourceBranchAfterMerge project
        then write "option is already enabled. Not doing anything" $> AlreadySet
        else do
          write "setting option"
          res <- enableSourceBranchDeletionAfterMrMerge (projectId project)
          case res of
            Left err -> write ("something went wrong. " <> show err) $> Error
            Right _ -> write "done" $> Set

enableSuccessfulPipelineForMergeRequirement :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => GroupId -> Sem r ()
enableSuccessfulPipelineForMergeRequirement gId = do
  write "=================================================="
  write $ "Enabling the requirement that a successful pipeline is required for a MR to be merged for Group " <> show gId
  getProjects gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse process projects
      write ""
      write "done: "
      let summary = foldl' (\m r -> M.insertWith (<>) r (Sum (1 :: Int)) m) (M.fromList $ (,mempty) <$> universe) res
      let summaryPrint = M.foldlWithKey' (\acc k (Sum c) -> (show k <> ": " <> show c) : acc) mempty summary
      traverse_ write summaryPrint
  where
    process :: (Member MergeRequestApi r, Member Writer r) => Project -> Sem r Result
    process project = do
      write ""
      write $ formatWith [bold] ("=== " <> show (name project))
      if onlyAllowMergeIfPipelineSucceeds project
        then write "option is already enabled. Not doing anything" $> AlreadySet
        else do
          write "setting option"
          res <- setSuccessfulPipelineRequirementForMerge (projectId project)
          case res of
            Left err -> write ("something went wrong. " <> show err) $> Error
            Right _ -> write "done" $> Set

enableAllDiscussionsResolvedForMergeRequirement :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => GroupId -> Sem r ()
enableAllDiscussionsResolvedForMergeRequirement gId = do
  write "=================================================="
  write $ "Enabling the requirement that all discussions must be resolved for a MR to be merged for Group " <> show gId
  getProjects gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse process projects
      write ""
      write "done: "
      let summary = foldl' (\m r -> M.insertWith (<>) r (Sum (1 :: Int)) m) (M.fromList $ (,mempty) <$> universe) res
      let summaryPrint = M.foldlWithKey' (\acc k (Sum c) -> (show k <> ": " <> show c) : acc) mempty summary
      traverse_ write summaryPrint
  where
    process :: (Member MergeRequestApi r, Member Writer r) => Project -> Sem r Result
    process project = do
      write ""
      write $ formatWith [bold] ("=== " <> show (name project))
      if onlyAllowMergeIfAllDiscussionsAreResolved project
        then write "option is already enabled. Not doing anything" $> AlreadySet
        else do
          write "setting option"
          res <- setResolvedDiscussionsRequirementForMerge (projectId project)
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

mkEnabledDisabledCount :: Bool -> EnabledDisabledCount a
mkEnabledDisabledCount b = if b then EnabledDisabledCount (Count 1, Count 0) else EnabledDisabledCount (Count 0, Count 1)

data ProjectCount

data SourceBranchDeletion

data HasNoDefaultBranch

data SuccessfulPipelineForMerge

data AllDiscussionsResolvedForMerge

type Summary =
  ( Count ProjectCount,
    EnabledDisabledCount SourceBranchDeletion,
    Count HasNoDefaultBranch,
    EnabledDisabledCount SuccessfulPipelineForMerge,
    EnabledDisabledCount AllDiscussionsResolvedForMerge
  )

writeSummary :: (Member Writer r) => Summary -> Sem r ()
writeSummary
  ( Count numProjects,
    EnabledDisabledCount (Count branchDeletionEnabled, Count branchDeletionDisabled),
    Count hasNoDefaultBranch,
    EnabledDisabledCount (Count successfulPipelineForMergeEnabled, Count successfulPipelineForMergeDisabled),
    EnabledDisabledCount (Count allDiscussionsResolvedForMergeEnabled, Count allDiscussionsResolvedForMergeDisabled)
    ) = do
    write ""
    write $ formatWith [bold] "=== Summary"
    write $ "Anzahl Projekte: " <> show numProjects
    write $ "Projekte die 'remove_source_branch_after_merge' aktiviert haben: " <> show branchDeletionEnabled
    write $ "Projekte die 'remove_source_branch_after_merge' NICHT aktiviert haben: " <> show branchDeletionDisabled
    write $ "Projekte die 'only_allow_merge_if_pipeline_succeeds' aktiviert haben: " <> show successfulPipelineForMergeEnabled
    write $ "Projekte die 'only_allow_merge_if_pipeline_succeeds' NICHT aktiviert haben: " <> show successfulPipelineForMergeDisabled
    write $ "Projekte die 'only_allow_merge_if_all_discussions_are_resolved' aktiviert haben: " <> show allDiscussionsResolvedForMergeEnabled
    write $ "Projekte die 'only_allow_merge_if_all_discussions_are_resolved' NICHT aktiviert haben: " <> show allDiscussionsResolvedForMergeDisabled
    write $ "Projekte ohne default branch: " <> show hasNoDefaultBranch

summarizeSingle :: Project -> Summary
summarizeSingle project =
  ( Count 1,
    sourceBranchDeletionEnabledDisabled,
    noDefaultBranch,
    successfulPipelineForMergeEnabledDisabled,
    allDiscussionsResolvedForMergeEnabledDisabled
  )
  where
    sourceBranchDeletionEnabledDisabled = mkEnabledDisabledCount branchDeletionEnabled
    branchDeletionEnabled = or (removeSourceBranchAfterMerge project)
    noDefaultBranch = Count $ if isJust (defaultBranch project) then 0 else 1
    successfulPipelineForMergeEnabledDisabled = mkEnabledDisabledCount $ onlyAllowMergeIfPipelineSucceeds project
    allDiscussionsResolvedForMergeEnabledDisabled = mkEnabledDisabledCount $ onlyAllowMergeIfAllDiscussionsAreResolved project
