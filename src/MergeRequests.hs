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

import Config.App (App)
import Config.Types (WithArchivedProjects (SkipArchivedProjects))
import Data.List (partition)
import qualified Data.Text as T (intercalate)
import Data.Time hiding (getCurrentTime)
import Effects
import Gitlab.Client.MTL (UpdateError)
import Gitlab.MergeRequest
import Gitlab.Project
import Relude

showMergeRequests :: App ()
showMergeRequests = do
  getProjectsForGroup SkipArchivedProjects >>= \case
    Left err -> write $ show err
    Right projects -> do
      let (mrEnabled, mrDisabled) = partition projectMergeRequestsEnabled projects
      printProjectsWithDisabledMergeRequests mrDisabled
      write ""
      printProjectsWithMergeRequests mrEnabled

printProjectsWithDisabledMergeRequests :: [Project] -> App ()
printProjectsWithDisabledMergeRequests projects = do
  write "=== projects with disabled merge requests"
  printProjects projects
  where
    printProjects :: [Project] -> App ()
    printProjects [] = write "There are no projects with disabled merge requests"
    printProjects ps = write $ T.intercalate ", " (show . projectName <$> ps)

printProjectsWithMergeRequests :: [Project] -> App ()
printProjectsWithMergeRequests projects = do
  write "=== projects with merge requests ( ⚬ = Draft)"
  printProjects projects
  where
    printProjects :: [Project] -> App ()
    printProjects [] = write "There are no projects with enabled merge requests"
    printProjects ps = do
      projectsWithMergeRequests <- traverse (\project -> (project,) <$> getOpenMergeRequests (projectId project) Nothing) ps
      let projectsWithOpenMergeRequests = filter hasOpenMergeRequests projectsWithMergeRequests
      mapM_ printProjectsWithMergeRequests' projectsWithOpenMergeRequests
    hasOpenMergeRequests (_, res) = (not . all null) res

printProjectsWithMergeRequests' :: (Project, Either UpdateError [MergeRequest]) -> App ()
printProjectsWithMergeRequests' (project, Left err) = write $ unwords ["Couldn't get open merge requests for", show project, "error was", show err]
printProjectsWithMergeRequests' (project, Right mrs) = do
  now <- getCurrentTime
  write $ prettyPrint project mrs now

prettyPrint :: Project -> [MergeRequest] -> UTCTime -> Text
prettyPrint project [] _ = unlines $ prettyPrintProject project <> ["no open merge requests"]
prettyPrint project mergeRequests now = unlines $ prettyPrintProject project <> (prettyPrintMergeRequest now <$> mergeRequests)

prettyPrintProject :: Project -> [Text]
prettyPrintProject project = ["----------", show (projectName project) <> " (" <> show (projectId project) <> ")"]

prettyPrintMergeRequest :: UTCTime -> MergeRequest -> Text
prettyPrintMergeRequest now MergeRequest {..} =
  (if mergeRequestWip then " ⚬ " else "   ")
    <> "#"
    <> show mergeRequestIid
    <> ":"
    <> (if mergeRequestConflicts then " has conflicts," else "")
    <> " opened "
    <> age now mergeRequestCreatedAt
    <> ", see: "
    <> show mergeRequestWebUrl

age :: UTCTime -> UTCTime -> Text
age now created =
  if
    | res == 0 -> "today"
    | res == 1 -> "yesterday"
    | otherwise -> show res <> " days ago"
  where
    res = fst . timeToDaysAndTimeOfDay $ diffUTCTime now created
