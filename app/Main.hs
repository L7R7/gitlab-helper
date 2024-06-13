{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), parseConfigOrDie)
import Effects (ProjectId (..))
import Interpreters
import Pipelines
import Util

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
      $ showPipelineDurationsForProject (ProjectId 795)

--      $ showBranchesForProject groupId
--      $ evaluateProjects groupId
