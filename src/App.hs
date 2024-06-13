{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App (run) where

import Branches (showBranchesForGroup)
import Config.Config (parseConfigOrDie)
import Config.Types
import Effects (ProjectId (..))
import Interpreters
import MergeRequests
import Pipelines
import Projects
import Relude
import Schedules (showSchedulesForGroup)
import Util

run :: IO ()
run = do
  Config {..} <- parseConfigOrDie
  command <- commandParser
  let action = case command of
        ShowBranches -> showBranchesForGroup groupId
        (EnableSourceBranchDeletionAfterMerge execution) -> enableSourceBranchDeletionAfterMerge execution groupId
        ShowProjects -> showProjectsForGroup groupId
        ShowSchedules -> showSchedulesForGroup groupId
        ShowMergeRequests -> showMergeRequests groupId
        (EnableAllDiscussionsMustBeResolvedForMergeRequirement execution) -> enableAllDiscussionsResolvedForMergeRequirement execution groupId
        (EnableSuccessfulPipelineForMergeRequirement execution) -> enableSuccessfulPipelineForMergeRequirement execution groupId
  runM
    . timerToIO
    . writerToIO
    . branchesApiToIO baseUrl apiToken
    . projectsApiToIO baseUrl apiToken
    . mergeRequestApiToIO baseUrl apiToken
    . pipelinesApiToIO baseUrl apiToken
    . schedulesApiToIO baseUrl apiToken
    $ action