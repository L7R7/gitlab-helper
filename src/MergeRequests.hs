{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module MergeRequests
  ( showMergeRequests,
  )
where

import Config.Types
import Data.List (partition)
import qualified Data.Text as T (intercalate)
import Data.Time hiding (getCurrentTime)
import Effects
import Polysemy
import Relude

showMergeRequests :: (Member ProjectsApi r, Member MergeRequestApi r, Member Timer r, Member Writer r) => GroupId -> Sem r ()
showMergeRequests gId = do
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      let (mrEnabled, mrDisabled) = partition mergeRequestsEnabled projects
      printProjectsWithDisabledMergeRequests mrDisabled
      write ""
      printProjectsWithMergeRequests mrEnabled

printProjectsWithDisabledMergeRequests :: (Member Writer r) => [Project] -> Sem r ()
printProjectsWithDisabledMergeRequests projects = do
  write "=== projects with disabled merge requests"
  printProjects projects
  where
    printProjects :: (Member Writer r) => [Project] -> Sem r ()
    printProjects [] = write "There are no projects with disabled merge requests"
    printProjects ps = write $ T.intercalate ", " (show . name <$> ps)

printProjectsWithMergeRequests :: (Member MergeRequestApi r, Member Timer r, Member Writer r) => [Project] -> Sem r ()
printProjectsWithMergeRequests projects = do
  write "=== projects with merge requests ( ⚬ = Draft)"
  printProjects projects
  where
    printProjects :: (Member MergeRequestApi r, Member Timer r, Member Writer r) => [Project] -> Sem r ()
    printProjects [] = write "There are no projects with enabled merge requests"
    printProjects ps = do
      projectsWithMergeRequests <- traverse (\project -> (project,) <$> getOpenMergeRequests (projectId project)) ps
      let projectsWithOpenMergeRequests = filter hasOpenMergeRequests projectsWithMergeRequests
      mapM_ printProjectsWithMergeRequests' projectsWithOpenMergeRequests
    hasOpenMergeRequests (_, res) = (not . all null) res

printProjectsWithMergeRequests' :: (Member Timer r, Member Writer r) => (Project, Either UpdateError [MergeRequest]) -> Sem r ()
printProjectsWithMergeRequests' (project, Left err) = write $ unwords ["Couldn't get open merge requests for", show project, "error was", show err]
printProjectsWithMergeRequests' (project, Right mrs) = do
  now <- getCurrentTime
  write $ prettyPrint project mrs now

prettyPrint :: Project -> [MergeRequest] -> UTCTime -> Text
prettyPrint project [] _ = unlines $ prettyPrintProject project <> ["no open merge requests"]
prettyPrint project mergeRequests now = unlines $ prettyPrintProject project <> (prettyPrintMergeRequest now <$> mergeRequests)

prettyPrintProject :: Project -> [Text]
prettyPrintProject project = ["----------", show (name project) <> " (" <> show (projectId project) <> ")"]

prettyPrintMergeRequest :: UTCTime -> MergeRequest -> Text
prettyPrintMergeRequest now MergeRequest {..} =
  (if wip then " ⚬ " else "   ")
    <> "#"
    <> show mergeRequestId
    <> ":"
    <> (if conflicts then " has conflicts," else "")
    <> " opened "
    <> age now createdAt
    <> ", see: "
    <> show webUrl

age :: UTCTime -> UTCTime -> Text
age now created =
  if
    | res == 0 -> "today"
    | res == 1 -> "yesterday"
    | otherwise -> show res <> " days ago"
  where
    res = fst . timeToDaysAndTimeOfDay $ diffUTCTime now created
