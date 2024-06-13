{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), parseConfigOrDie)
import Interpreters
import Lib
import Util
import Branches
main :: IO ()
main =
  parseConfigOrDie >>= \Config {..} ->
    runM
      . timerToIO
      . writerToIO
      . branchesApiToIO baseUrl apiToken
      . projectsApiToIO baseUrl apiToken
      $ showBranchesForProject groupId
--      . mergeRequestApiToIO baseUrl apiToken
--      $ evaluateProjects groupId
