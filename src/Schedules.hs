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

module Schedules
  ( showSchedulesForGroup,
  )
where

import App (App)
import Colourista.Pure
import Config.Types (Config (..), WithArchivedProjects (SkipArchivedProjects))
import Effects
import Gitlab.Client.Queue.MTL
import Gitlab.Lib (Name (..))
import Gitlab.Project
import Relude

showSchedulesForGroup :: App ()
showSchedulesForGroup = do
  gId <- asks groupId
  write "=================================================="
  write $ "Listing the projects' schedules for Group " <> show gId
  processProjectsForGroupQueued SkipArchivedProjects (fmap (Right . Result) . getSchedulesForProject) >>= \case
    Left err -> write $ show err
    Right res -> do
      traverse_ printResults (sortOn (getName . projectName . fst) res)
      writeSummary res

getSchedulesForProject :: Project -> App (Project, Either UpdateError [Schedule])
getSchedulesForProject p = (p,) <$> getSchedules (projectId p)

printResults :: (Project, Either UpdateError [Schedule]) -> App ()
printResults (project, Left err) = do
  write $ formatWith [bold] ("=== " <> show (projectName project))
  write $ "something went wrong: " <> show err
printResults (_, Right []) = pure ()
printResults (project, Right schedules) = do
  write ""
  write $ formatWith [bold] ("=== " <> show (projectName project))
  traverse_ (write . prettyPrintSchedule) schedules

prettyPrintSchedule :: Schedule -> Text
prettyPrintSchedule Schedule {..} =
  show scheduleId
    <> " - "
    <> scheduleDescription
    <> ": "
    <> scheduleCron
    <> " "
    <> scheduleCronTimezone
    <> ", next run at "
    <> show scheduleNextRunAt
    <> ", is "
    <> (if scheduleActive then "enabled" else "disabled")
    <> ", owned by "
    <> scheduleOwner

type ProjectCount = Sum Int

type ErrorCount = Sum Int

type SchedulesCount = Sum Int

type ProjectsWithoutSchedule = Sum Int

type ProjectsWithSchedules = Sum Int

writeSummary :: [(Project, Either UpdateError [Schedule])] -> App ()
writeSummary results = do
  write ""
  write . showSummary $ summary results

summary :: [(Project, Either UpdateError [Schedule])] -> (ProjectCount, SchedulesCount, ProjectsWithoutSchedule, ProjectsWithSchedules, ErrorCount)
summary = foldMap count

showSummary :: (ProjectCount, SchedulesCount, ProjectsWithoutSchedule, ProjectsWithSchedules, ErrorCount) -> Text
showSummary (projects, schedules, withoutSchedule, withSchedules, errors) =
  formatWith [bold]
    $ unwords
      [ " ▶",
        show . getSum $ schedules,
        "schedules in",
        show . getSum $ projects,
        "projects.",
        show . getSum $ withSchedules,
        "of the projects have schedules,",
        show . getSum $ withoutSchedule,
        "don't have schedules.",
        "Update for",
        show . getSum $ errors,
        "project" <> (if getSum errors == 1 then "" else "s"),
        "failed."
      ]

count :: (Project, Either UpdateError [Schedule]) -> (ProjectCount, SchedulesCount, ProjectsWithoutSchedule, ProjectsWithSchedules, ErrorCount)
count (_, Left _) = (Sum 1, mempty, mempty, mempty, Sum 1)
count (_, Right schedules) = (Sum 1, schedulesCount, withoutSchedulesCount, withSchedulesCount, mempty)
  where
    schedulesCount = Sum $ length schedules
    withoutSchedulesCount = if null schedules then Sum 1 else mempty
    withSchedulesCount = if null schedules then mempty else Sum 1
