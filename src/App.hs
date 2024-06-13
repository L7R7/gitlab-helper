{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App (run) where

import Branches (showBranchesForGroup)
import Config.Config (parseConfigOrDie)
import Config.Types
import Interpreters
import MergeRequests
import Projects
import Relude
import Schedules (showSchedulesForGroup)
import UpdateMergeRequests (updateMergeRequests)
import Util

run :: IO ()
run = do
  Config {..} <- parseConfigOrDie
  let program = case cmd of
        ShowBranches -> showBranchesForGroup groupId
        (EnableSourceBranchDeletionAfterMerge execution) -> enableSourceBranchDeletionAfterMerge execution groupId
        ShowProjects -> showProjectsForGroup groupId
        ListAllProjectsMeta -> listAllProjectsMeta
        ListProjectsMeta -> listProjectsMetaForGroup groupId
        ShowSchedules -> showSchedulesForGroup groupId
        ShowMergeRequests -> showMergeRequests groupId
        (EnableAllDiscussionsMustBeResolvedForMergeRequirement execution) -> enableAllDiscussionsResolvedForMergeRequirement execution groupId
        (EnableSuccessfulPipelineForMergeRequirement execution) -> enableSuccessfulPipelineForMergeRequirement execution groupId
        (CountSuccessfulDeployments year) -> countDeployments groupId year
        (SetMergeMethodToFastForward execution) -> setMergeMethodToFastForward execution groupId
        (UpdateMergeRequests action authorIs searchTerm execution) -> updateMergeRequests groupId action authorIs searchTerm execution
  runM
    . timerToIO
    . writerToIO
    . branchesApiToIO baseUrl apiToken
    . usersApiToIO baseUrl apiToken
    . groupsApiToIO baseUrl apiToken
    . projectsApiToIO baseUrl apiToken
    . mergeRequestApiToIO baseUrl apiToken
    . pipelinesApiToIO baseUrl apiToken
    . schedulesApiToIO baseUrl apiToken
    $ program
