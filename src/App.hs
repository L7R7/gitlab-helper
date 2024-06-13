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
import Schedules (showSchedulesForGroup)
import Util
import Prelude

run :: IO ()
run = do
  Config {..} <- parseConfigOrDie
  command <- commandParser
  let action = case command of
        ShowBranches -> showBranchesForGroup groupId
        EnableSourcBranchDeletionAfterMerge -> enableSourceBranchDeletionAfterMerge groupId
        ShowProjects -> showProjectsForGroup groupId
        ShowSchedules -> showSchedulesForGroup groupId
        ShowPipelineDurations -> error "not implemented"
        ShowMergeRequests -> showMergeRequests groupId
  runM
    . timerToIO
    . writerToIO
    . branchesApiToIO baseUrl apiToken
    . projectsApiToIO baseUrl apiToken
    . mergeRequestApiToIO baseUrl apiToken
    . pipelinesApiToIO baseUrl apiToken
    . schedulesApiToIO baseUrl apiToken
    . writeToFileToIO
    $ action
