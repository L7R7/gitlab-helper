{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpdateMergeRequests
  ( updateMergeRequests,
  )
where

import Config.Types
import Effects hiding (Merge)
import Polysemy
import Relude

updateMergeRequests ::
  forall r.
  (Member MergeRequestApi r, Member Writer r) =>
  GroupId ->
  MergeRequestUpdateAction ->
  AuthorIs ->
  Maybe SearchTerm ->
  Execution ->
  Sem r ()
updateMergeRequests _ Merge _ Nothing Execute =
  write "I don't think you want to blindly merge all merge requests for this group. Consider adding a filter. Exiting now."
updateMergeRequests gId action authorIs searchTerm execute = do
  getOpenMergeRequestsForGroup gId (Just authorIs) searchTerm >>= \case
    Left err -> write $ show err
    Right [] -> write "no MRs to process"
    Right mergeRequests -> forM_ mergeRequests $ \mr -> do
      performAction (mergeRequestProjectId mr) mr >>= \case
        Left err -> write $ show err
        Right _ -> pure ()
  where
    performAction pId mr = do
      write $ "processing MR #" <> show (mergeRequestId mr) <> " in Project #" <> show (mergeRequestProjectId mr) <> ": " <> mergeRequestTitle mr
      let f = case execute of
            DryRun -> performActionDry
            Execute -> performActionExecute
      f pId (mergeRequestId mr)

    performActionExecute = case action of
      Rebase -> rebaseMergeRequest
      Merge -> mergeMergeRequest

    performActionDry _ _ =
      Right () <$ case action of
        Rebase -> write "dry run. skipping rebase"
        Merge -> write "dry run. skipping merge"
