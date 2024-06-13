{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Projects
  ( showProjectsForGroup,
    enableSourceBranchDeletionAfterMerge,
  )
where

import Colourista (bold, formatWith)
import Config
import qualified Data.Map as M
import Effects
import Polysemy
import Relude
import Relude.Extra (universe)

showProjectsForGroup :: (Member ProjectsApi r, Member Writer r) => GroupId -> Sem r ()
showProjectsForGroup gId = do
  write "=================================================="
  write $ "Listing the projects for Group " <> show gId
  getProjects gId >>= \case
    Left err -> write $ show err
    Right projects -> traverse_ (write . show) projects

enableSourceBranchDeletionAfterMerge :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => GroupId -> Sem r ()
enableSourceBranchDeletionAfterMerge gId = do
  write "=================================================="
  write $ "Enabling automatic branch deletion after MR merge for Group " <> show gId
  getProjects gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse process projects
      write ""
      write "done: "
      let summary = foldl' (\m r -> M.insertWith (<>) r (Sum (1 :: Int)) m) (M.fromList $ (,mempty) <$> universe) res
      let summaryPrint = M.foldlWithKey' (\acc k (Sum c) -> (show k <> ": " <> show c) : acc) mempty summary
      traverse_ write summaryPrint

process :: (Member MergeRequestApi r, Member Writer r) => Project -> Sem r Result
process project = do
  write ""
  write $ formatWith [bold] ("=== " <> show (name project))
  if Just True == removeSourceBranchAfterMerge project
    then write "option is already enabled. Not doing anything" $> AlreadySet
    else
      ( do
          write "setting option"
          res <- enableSourceBranchDeletionAfterMrMerge (projectId project)
          case res of
            Left err -> write ("something went wrong. " <> show err) $> Error
            Right _ -> write "done" $> Set
      )

data Result = AlreadySet | Set | Error deriving (Bounded, Enum, Eq, Ord, Show)
