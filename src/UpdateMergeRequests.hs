{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module UpdateMergeRequests
  ( updateMergeRequests,
  )
where

import Config.Types
import qualified Data.Text as T (isInfixOf)
import Effects hiding (Merge)
import Polysemy
import Relude

updateMergeRequests ::
  forall r.
  (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) =>
  GroupId ->
  MergeRequestUpdateAction ->
  AuthorIs ->
  Maybe MergeRequestTitleFilter ->
  Maybe MergeRequestDescriptionFilter ->
  Execution ->
  Sem r ()
updateMergeRequests _ Merge _ Nothing Nothing Execute =
  write "I don't think you want to blindly merge all merge requests for this group. Exiting now."
updateMergeRequests gId action authorIs titleFilter descriptionFilter execute = do
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      mergeRequestsResults <- traverse (\project -> (project,) <$> getOpenMergeRequests (projectId project) (Just authorIs)) projects
      forM_ mergeRequestsResults $ \case
        (p, Left err') -> write $ "failed to process project " <> show (projectId p) <> ": " <> show err'
        (_, Right []) -> pure ()
        (p, Right mergeRequests) -> do
          write $ "=== Project " <> show (projectId p)
          traverse_ (processMergeRequest p) mergeRequests
  where
    processMergeRequest :: Project -> MergeRequest -> Sem r ()
    processMergeRequest p mr = do
      if includeMergeRequest mr
        then do
          performAction p mr >>= \case
            Left err -> write $ show err
            Right _ -> pure ()
        else write $ "Skipping " <> show (mergeRequestId mr) <> ", filter did not apply"

    includeMergeRequest (MergeRequest _ mrTitle mrDescription _ _ _ _) =
      maybe True (\(TitleContains s) -> fromString s `T.isInfixOf` mrTitle) titleFilter
        && maybe True (\(DescriptionContains s) -> fromString s `T.isInfixOf` mrDescription) descriptionFilter

    performAction p mr = do
      write $ "processing MR #" <> show (mergeRequestId mr)
      let f = case execute of
            DryRun -> performActionDry
            Execute -> performActionExecute
      f (projectId p) (mergeRequestId mr)

    performActionExecute = case action of
      Rebase -> rebaseMergeRequest
      Merge -> mergeMergeRequest

    performActionDry _ _ =
      Right () <$ case action of
        Rebase -> write "dry run. skipping rebase"
        Merge -> write "dry run. skipping merge"
