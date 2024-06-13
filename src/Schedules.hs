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

import Colourista.Pure
import Config.Types
import Effects
import Polysemy
import Relude

showSchedulesForGroup :: (Member ProjectsApi r, Member SchedulesApi r, Member Writer r) => GroupId -> Sem r ()
showSchedulesForGroup gId = do
  write "=================================================="
  write $ "Listing the projects' schedules for Group " <> show gId
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      results <- traverse getSchedulesForProject (sortOn name projects)
      traverse_ printResults results
      writeSummary results

getSchedulesForProject :: (Member SchedulesApi r) => Project -> Sem r (Project, Either UpdateError [Schedule])
getSchedulesForProject p = (p,) <$> getSchedules (projectId p)

printResults :: Member Writer r => (Project, Either UpdateError [Schedule]) -> Sem r ()
printResults (project, Left err) = do
  write $ formatWith [bold] ("=== " <> show (name project))
  write $ "something went wrong: " <> show err
printResults (_, Right []) = pure ()
printResults (project, Right schedules) = do
  write ""
  write $ formatWith [bold] ("=== " <> show (name project))
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

writeSummary :: Member Writer r => [(Project, Either UpdateError [Schedule])] -> Sem r ()
writeSummary results = do
  write ""
  write . showSummary $ summary results

summary :: [(Project, Either UpdateError [Schedule])] -> (ProjectCount, SchedulesCount, ProjectsWithoutSchedule, ProjectsWithSchedules, ErrorCount)
summary = foldMap count

showSummary :: (ProjectCount, SchedulesCount, ProjectsWithoutSchedule, ProjectsWithSchedules, ErrorCount) -> Text
showSummary (projects, schedules, withoutSchedule, withSchedules, errors) =
  formatWith [bold] $
    unwords
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
