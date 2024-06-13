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
import Data.Text (isInfixOf, strip, stripPrefix, toLower)
import Effects
import Gitlab.Group
import Gitlab.Lib (Id)
import Gitlab.MergeRequest
import Gitlab.Project (Project)
import Polysemy
import Relude

updateMergeRequests ::
  forall r.
  (Member MergeRequestApi r, Member Writer r) =>
  Id Group ->
  [Id Project] ->
  MergeRequestUpdateAction ->
  AuthorIs ->
  Maybe (Either SearchTerm SearchTermTitle) ->
  Execution ->
  Sem r ()
updateMergeRequests _ _ Merge _ Nothing Execute =
  write "I don't think you want to blindly merge all merge requests for this group. Consider adding a filter. Exiting now."
updateMergeRequests gId projectExcludes action authorIs maybeSearchTerms execute = do
  let searchTerm' = either id (\(SearchTermTitle s) -> SearchTerm s) <$> maybeSearchTerms
  getOpenMergeRequestsForGroup gId (Just authorIs) searchTerm' >>= \case
    Left err -> write $ show err
    Right [] -> write "no MRs to process"
    Right allMergeRequests -> do
      let titleFilter mr = case maybeSearchTerms of
            Just (Right (SearchTermTitle s)) -> toLower (toText s) `isInfixOf` toLower (mergeRequestTitle mr)
            _ -> True
          filteredMergeRequests = filter (\mr -> titleFilter mr && mergeRequestProjectId mr `notElem` projectExcludes) allMergeRequests
      case filteredMergeRequests of
        [] -> write "no MRs to process after applying filters"
        mergeRequests -> forM_ mergeRequests $ \mr -> do
          performAction (mergeRequestProjectId mr) mr >>= \case
            Left err -> write $ show err
            Right _ -> pure ()
  where
    performAction pId mr = do
      write $ "processing MR #" <> show (mergeRequestIid mr) <> " in Project #" <> show (mergeRequestProjectId mr) <> ": " <> mergeRequestTitle mr
      let f = case execute of
            DryRun -> performActionDry
            Execute -> performActionExecute
      f pId mr

    printMergeRequest _ = pure ()

    performActionExecute pId mr = case action of
      List -> Right () <$ printMergeRequest mr
      Rebase -> rebaseMergeRequest pId (mergeRequestIid mr)
      Merge -> mergeMergeRequest pId (mergeRequestIid mr)
      SetToDraft ->
        if mergeRequestWip mr
          then Right () <$ write "merge request is already in state \"Draft\""
          else setMergeRequestTitle pId (mergeRequestIid mr) ("Draft: " <> mergeRequestTitle mr)
      MarkAsReady ->
        if mergeRequestWip mr
          then setMergeRequestTitle pId (mergeRequestIid mr) (strip $ fromMaybe (mergeRequestTitle mr) (stripPrefix "Draft:" (mergeRequestTitle mr)))
          else Right () <$ write "merge request is already marked as ready"

    performActionDry _ mr =
      Right () <$ case action of
        List -> printMergeRequest mr
        Rebase -> write "dry run. skipping rebase"
        Merge -> write "dry run. skipping merge"
        SetToDraft ->
          if mergeRequestWip mr
            then write "merge request is already in state \"Draft\""
            else write "dry run. skipping draft toggle"
        MarkAsReady ->
          if mergeRequestWip mr
            then write "dry run. skipping draft toggle"
            else write "merge request is already marked as ready"
