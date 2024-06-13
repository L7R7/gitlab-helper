{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App (run) where

import Branches (showBranchesForGroup)
import Config.Config (parseConfigOrDie)
import Config.Types
import Interpreters
import MergeRequests
import qualified Polysemy.Reader as R
import Projects
import Relude
import Schedules (showSchedulesForGroup)
import UpdateMergeRequests (updateMergeRequests)
import Util

run :: IO ()
run = do
  c@Config {..} <- parseConfigOrDie
  -- putStrLn $ "running with config: " <> show c
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
        (UpdateMergeRequests action authorIs searchTerm execution) -> updateMergeRequests groupId projectsExcludeList action authorIs searchTerm execution
  runM
    . timerToIO
    . writerToIO
    . R.runReader c
    . branchesApiToIO baseUrl apiToken
    . usersApiToIO baseUrl apiToken
    . groupsApiToIO baseUrl apiToken
    . projectsApiToIO baseUrl apiToken
    . mergeRequestApiToIO baseUrl apiToken
    . pipelinesApiToIO baseUrl apiToken
    . schedulesApiToIO baseUrl apiToken
    $ program
