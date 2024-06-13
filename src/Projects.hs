{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Projects
  ( showProjectsForGroup,
    enableSourceBranchDeletionAfterMerge,
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

data Result = AlreadySet | Set | Error deriving (Bounded, Enum, Eq, Ord, Show)

newtype ProjectCount = ProjectCount Int
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

newtype SourceBranchDeletionEnabled = SourceBranchDeletionEnabled Int
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

newtype SourceBranchDeletionDisabled = SourceBranchDeletionDisabled Int
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

newtype HasNoDefaultBranch = HasNoDefaultBranch Int
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

type Summary = (ProjectCount, SourceBranchDeletionEnabled, SourceBranchDeletionDisabled, HasNoDefaultBranch)

writeSummary :: (Member Writer r) => Summary -> Sem r ()
writeSummary (ProjectCount numProjects, SourceBranchDeletionEnabled branchDeletionEnabled, SourceBranchDeletionDisabled branchDeletionDisabled, HasNoDefaultBranch hasNoDefaultBranch) = do
  write ""
  write $ formatWith [bold] "=== Summary"
  write $ "Anzahl Projekte: " <> show numProjects
  write $ "Projekte die 'remove_source_branch_after_merge' aktiviert haben: " <> show branchDeletionEnabled
  write $ "Projekte die 'remove_source_branch_after_merge' NICHT aktiviert haben: " <> show branchDeletionDisabled
  write $ "Projekte ohne default branch: " <> show hasNoDefaultBranch

summarizeSingle :: Project -> Summary
summarizeSingle project = (ProjectCount 1, sourceBranchDeletionEnabled, sourceBranchDeletionDisabled, noDefaultBranch)
  where
    sourceBranchDeletionEnabled = SourceBranchDeletionEnabled $ if branchDeletionEnabled then 1 else 0
    sourceBranchDeletionDisabled = SourceBranchDeletionDisabled $ if branchDeletionEnabled then 0 else 1
    branchDeletionEnabled = or (removeSourceBranchAfterMerge project)
    noDefaultBranch = HasNoDefaultBranch $ if isJust (defaultBranch project) then 0 else 1
