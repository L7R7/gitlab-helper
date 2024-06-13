{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Branches (showBranchesForProject)
import Config (Config (..), parseConfigOrDie)
import Effects (ProjectId (..))
import Interpreters
import Pipelines
import Util
import Prelude

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
      . writeToFileToIO
      --      $ showPipelineDurationsForProject (ProjectId 795) -- 818) -- 795)

      $ showBranchesForProject groupId

--      $ evaluateProjects groupId
