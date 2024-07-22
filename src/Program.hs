{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Program (run) where

import App
import Branches (showBranchesForGroup)
import Config.Config (parseConfigOrDie)
import Config.Types
import Effects (write)
import GitHash
import MergeRequests
import Projects
import Relude
import Schedules (showSchedulesForGroup)
import UpdateMergeRequests (updateMergeRequests)

run :: IO ()
run = do
  c@Config {..} <- parseConfigOrDie
  let gitCommit = "Version: " <> fromString (giTag $$tGitInfoCwd)
  -- putStrLn $ "running with config: " <> show c
  let program = case cmd of
        Version -> write gitCommit
        ShowBranches -> showBranchesForGroup
        (EnableSourceBranchDeletionAfterMerge execution) -> enableSourceBranchDeletionAfterMerge execution
        ShowProjects -> showProjectsForGroup
        ListProjectsMeta scope -> listProjectsMeta scope
        ShowSchedules -> showSchedulesForGroup
        (ShowMergeRequests recheckMergeStatus) -> showMergeRequests recheckMergeStatus
        (EnableAllDiscussionsMustBeResolvedForMergeRequirement execution) -> enableAllDiscussionsResolvedForMergeRequirement execution
        (EnableSuccessfulPipelineForMergeRequirement execution) -> enableSuccessfulPipelineForMergeRequirement execution
        (CountSuccessfulDeployments year withArchivedProjects) -> countDeployments year withArchivedProjects
        (SetMergeMethodToFastForward execution) -> setMergeMethodToFastForward execution
        (UpdateMergeRequests action authorIs searchTerm recheckMergeStatus execution) -> updateMergeRequests projectsExcludeList action authorIs searchTerm recheckMergeStatus execution
  runReaderT (unApp program) c
