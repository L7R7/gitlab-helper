{-# LANGUAGE RecordWildCards #-}

module Program (run) where

import App
import Branches (showBranchesForGroup)
import Config
import MergeRequests
import OptEnvConf
import Paths_gitlab_helper (version)
import Projects
import Relude
import Schedules (showSchedulesForGroup)
import UpdateMergeRequests (updateMergeRequests)

run :: IO ()
run = do
  c@Config {..} <- runSettingsParser version "Some utilities for working with GitLab to make your life easier."
  -- putStrLn $ "running with config: " <> show c
  let program = case cmd of
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
