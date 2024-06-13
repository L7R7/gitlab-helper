{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Branches
  ( showBranchesForGroup,
  )
where

import Colourista.Pure
import qualified Data.Text as T (intercalate)
import Data.Time hiding (getCurrentTime)
import Effects
import Gitlab.Branch
import Gitlab.Client (UpdateError)
import Gitlab.Group
import Gitlab.Lib (Id)
import Gitlab.Project
import Polysemy
import Relude

showBranchesForGroup :: (Member ProjectsApi r, Member BranchesApi r, Member Timer r, Member Writer r) => Id Group -> Sem r ()
showBranchesForGroup gId = do
  write "=================================================="
  write $ "Listing the projects' branches for Group " <> show gId
  write "Only those projects that have branches other than the default branch will be printed"
  write "The icons at the start of the line indicate the following:"
  write "  ✔ - the branch is merged"
  write "  ✗ - the branch is stale (older than 90 days)"
  write "  ⚬ - the branch is protected"
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      results <- traverse (getBranchesForProject >=> printResult) projects
      writeSummary results

getBranchesForProject :: (Member BranchesApi r) => Project -> Sem r (Project, Either UpdateError [Branch])
getBranchesForProject p = (p,) <$> getBranches (projectId p)

printResult :: (Member Writer r, Member Timer r) => (Project, Either UpdateError [Branch]) -> Sem r (Project, Either UpdateError [Branch])
printResult input@(project, Left err) = do
  write $ "=== " <> show (projectName project)
  write $ "something went wrong: " <> show err
  pure input
printResult input@(project, Right branches) = do
  let branchesWithoutDefaultBranch = sortOn (commitCommittedDate . branchCommit) $ filter (not . branchDefault) branches
  unless (null branchesWithoutDefaultBranch) $ do
    write ""
    write $ formatWith [bold] ("=== " <> show (projectName project))
    now <- getCurrentTime
    traverse_ (\b -> write $ " " <> prettyPrintBranch now b) branchesWithoutDefaultBranch
  pure input

prettyPrintBranch :: UTCTime -> Branch -> Text
prettyPrintBranch now Branch {..} =
  prefix
    <> show branchName
    <> ": "
    <> T.intercalate ", " [prettyPrintAge ageDays, "see: " <> show branchWebUrl]
  where
    mergedPrefix = if branchMerged then "✔" else " "
    stalePrefix = if ageDays > 90 then "✗" else " "
    protectedPrefix = if branchProtected then "⚬" else " "
    prefix = unwords [mergedPrefix, stalePrefix, protectedPrefix, ""]
    ageDays = age now (commitCommittedDate branchCommit)

prettyPrintAge :: Integer -> Text
prettyPrintAge = unwords . reverse . go []
  where
    go :: [Text] -> Integer -> [Text]
    go acc n
      | n >= 365 = let (years, days) = divMod n 356 in go ((show years <> "y") : acc) days
      | n >= 30 = let (months, days) = divMod n 30 in go ((show months <> "m") : acc) days
      | otherwise = show n <> "d" : acc

age :: UTCTime -> UTCTime -> Integer
age now created = fst . timeToDaysAndTimeOfDay $ diffUTCTime now created

type ProjectCount = Sum Int

type BranchesCount = Sum Int

type StaleBranchesCount = Sum Int

type MergedBranchesCount = Sum Int

type Summary = (ProjectCount, BranchesCount, StaleBranchesCount, MergedBranchesCount)

writeSummary :: (Member Writer r, Member Timer r) => [(Project, Either UpdateError [Branch])] -> Sem r ()
writeSummary results = do
  now <- getCurrentTime
  write ""
  write . showSummary $ summary now results

summary :: UTCTime -> [(Project, Either UpdateError [Branch])] -> Summary
summary now = foldMap (count now)

showSummary :: Summary -> Text
showSummary (projects, branches, stale, merged) =
  formatWith [bold]
    $ unwords
      [ " ▶",
        show . getSum $ branches,
        "branches in",
        show . getSum $ projects,
        "projects.",
        show . getSum $ stale,
        "of them " <> isAre stale <> " stale,",
        show . getSum $ merged,
        "of them " <> isAre merged <> " merged."
      ]
  where
    isAre (Sum 1) = "is"
    isAre _ = "are"

count :: UTCTime -> (Project, Either UpdateError [Branch]) -> Summary
count _ (_, Left _) = mempty
count now (_, Right branches) = (hasBranches, notDefaultCount, stale, merged)
  where
    notDefault = filter (not . branchDefault) branches
    hasBranches = Sum $ if notDefaultCount /= 0 then 1 else 0
    (notDefaultCount, stale, merged) = foldMap (\b -> (countBranch b, isStale now b, isMerged b)) notDefault

isStale :: UTCTime -> Branch -> StaleBranchesCount
isStale now branch = Sum $ if age now ((commitCommittedDate . branchCommit) branch) > 90 then 1 else 0

isMerged :: Branch -> MergedBranchesCount
isMerged branch = Sum $ if branchMerged branch then 1 else 0

countBranch :: Branch -> BranchesCount
countBranch _ = Sum 1
