{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Branches (showBranchesForGroup)
import Config.Config (parseConfigOrDie)
import Config.Types(Config(..))
import Effects (ProjectId (..))
import Interpreters
import Pipelines
import Projects
import Util
import Prelude
import Schedules (showSchedulesForGroup)

main :: IO ()
main =
  parseConfigOrDie >>= \Config {..} ->
    runM
      . timerToIO
      . writerToIO
      . branchesApiToIO baseUrl apiToken
      . projectsApiToIO baseUrl apiToken
      . mergeRequestApiToIO baseUrl apiToken
      . pipelinesApiToIO baseUrl apiToken
      . schedulesApiToIO baseUrl apiToken
      . writeToFileToIO
           $ showSchedulesForGroup groupId
      --      $ showPipelineDurationsForProject (ProjectId 795) -- 818) -- 795)
      --      $ showProjectsForGroup groupId
      --      $ evaluateProjects groupId
      -- $ enableSourceBranchDeletionAfterMerge groupId
