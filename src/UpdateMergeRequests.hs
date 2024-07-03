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

import App (App)
import Config.Types
import Data.Text (isInfixOf, strip, stripPrefix, toLower)
import Effects
import Gitlab.Lib (Id)
import Gitlab.MergeRequest
import Gitlab.Project (Project)
import Relude

updateMergeRequests ::
  [Id Project] ->
  MergeRequestUpdateAction ->
  Maybe AuthorIs ->
  Maybe (Either SearchTerm SearchTermTitle) ->
  MergeStatusRecheck ->
  Execution ->
  App ()
updateMergeRequests _ (Merge _) _ Nothing _ Execute =
  write "I don't think you want to blindly merge all merge requests for this group. Consider adding a filter. Exiting now."
updateMergeRequests projectExcludes action authorIs maybeSearchTerms recheckMergeStatus execute = do
  let searchTerm' = either id (\(SearchTermTitle s) -> SearchTerm s) <$> maybeSearchTerms
  getOpenMergeRequestsForGroup authorIs searchTerm' recheckMergeStatus >>= \case
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
            Left err -> write $ "failed to update merge request: " <> show err
            Right _ -> pure ()
  where
    performAction pId mr = do
      write $ "processing MR #" <> show (mergeRequestIid mr) <> " in Project #" <> show (mergeRequestProjectId mr) <> " with state " <> show (mergeRequestDetailedMergeStatus mr) <> ": " <> mergeRequestTitle mr
      let f = case execute of
            DryRun -> performActionDry
            Execute -> performActionExecute
      f pId mr

    printMergeRequest _ = pure ()

    performActionExecute pId mr = case action of
      List -> Right () <$ printMergeRequest mr
      Rebase -> rebaseMergeRequest pId (mergeRequestIid mr)
      -- todo: get rid of the code duplication for finding the MergeDecision?
      (Merge mergeCiOption) -> case detailedMergeStatusToDecision (mergeRequestDetailedMergeStatus mr) of
        MergeShouldWork -> mergeMergeRequest pId (mergeRequestIid mr) mergeCiOption
        MergeMayWork -> mergeMergeRequest pId (mergeRequestIid mr) mergeCiOption
        MergeWontWork -> Right () <$ write ("The merge status is " <> show (mergeRequestDetailedMergeStatus mr) <> ", skipping the merge as it wouldn't succeed")
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
        -- todo: get rid of the code duplication for finding the MergeDecision?
        (Merge _) -> case detailedMergeStatusToDecision (mergeRequestDetailedMergeStatus mr) of
          MergeShouldWork -> write "dry run. skipping merge"
          MergeMayWork -> write "dry run. skipping merge attempt"
          MergeWontWork -> write ("The merge status is " <> show (mergeRequestDetailedMergeStatus mr) <> ", skipping the merge as it wouldn't succeed")
        SetToDraft ->
          if mergeRequestWip mr
            then write "merge request is already in state \"Draft\""
            else write "dry run. skipping draft toggle"
        MarkAsReady ->
          if mergeRequestWip mr
            then write "dry run. skipping draft toggle"
            else write "merge request is already marked as ready"

    detailedMergeStatusToDecision :: DetailedMergeStatus -> MergeDecision
    detailedMergeStatusToDecision ApprovalsSyncing = MergeMayWork
    detailedMergeStatusToDecision BlockedStatus = MergeWontWork
    detailedMergeStatusToDecision Checking = MergeMayWork
    detailedMergeStatusToDecision CIMustPass = MergeWontWork
    detailedMergeStatusToDecision CIStillRunning = MergeMayWork
    detailedMergeStatusToDecision Conflict = MergeWontWork
    detailedMergeStatusToDecision DiscussionsNotResolved = MergeWontWork
    detailedMergeStatusToDecision DraftStatus = MergeWontWork
    detailedMergeStatusToDecision ExternalStatusChecks = MergeWontWork
    detailedMergeStatusToDecision JiraAssociationMissing = MergeWontWork
    detailedMergeStatusToDecision Mergeable = MergeShouldWork
    detailedMergeStatusToDecision NeedRebase = MergeWontWork
    detailedMergeStatusToDecision NotApproved = MergeWontWork
    detailedMergeStatusToDecision NotOpen = MergeWontWork
    detailedMergeStatusToDecision RequestedChanges = MergeWontWork
    detailedMergeStatusToDecision Unchecked = MergeMayWork

-- | Depending on the merge status of a merge request, trying a merge may or may not make sense.
-- There are cases where a merge will very likely be successful, others where it maybe will work, and others where it definitely won't work.
--
-- This is roughly equivalent to the question whether the UI says "merge", "set to auto-merge", or "a merge is not possible"
--
-- This type represents these different cases.
data MergeDecision = MergeShouldWork | MergeMayWork | MergeWontWork
