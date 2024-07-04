{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UpdateMergeRequests
  ( updateMergeRequests,
  )
where

import App (App)
import Config
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
          write $ "processing MR #" <> show (mergeRequestIid mr) <> " in Project #" <> show (mergeRequestProjectId mr) <> " with state " <> show (mergeRequestDetailedMergeStatus mr) <> ": " <> mergeRequestTitle mr
          res <- performAction mr
          case res of
            Left err -> write $ "failed to update merge request: " <> show err
            Right _ -> pure ()
  where
    performAction mr =
      let pId = mergeRequestProjectId mr
       in case action of
            List -> pure $ Right ()
            Rebase -> rebaseAction pId (mergeRequestIid mr)
            (Merge mergeCiOption) -> case detailedMergeStatusToDecision (mergeRequestDetailedMergeStatus mr) of
              MergeShouldWork -> mergeAction pId (mergeRequestIid mr) mergeCiOption
              MergeMayWork -> mergeAttemptAction pId (mergeRequestIid mr) mergeCiOption
              MergeWontWork -> mergeWontWorkAction (mergeRequestDetailedMergeStatus mr)
            SetToDraft ->
              if mergeRequestWip mr
                then Right () <$ write "merge request is already in state \"Draft\""
                else setToDraftAction pId (mergeRequestIid mr) (mergeRequestTitle mr)
            MarkAsReady ->
              if mergeRequestWip mr
                then markAsReadyAction pId (mergeRequestIid mr) (mergeRequestTitle mr)
                else Right () <$ write "merge request is already marked as ready"

    (rebaseAction, mergeAction, mergeAttemptAction, mergeWontWorkAction, setToDraftAction, markAsReadyAction) = case execute of
      Execute ->
        ( rebaseMergeRequest,
          mergeMergeRequest,
          mergeMergeRequest,
          \detailedStatus -> Right () <$ write ("The merge status is " <> show detailedStatus <> ", skipping the merge as it wouldn't succeed"),
          \pId mrIid mrTitle -> setMergeRequestTitle pId mrIid ("Draft: " <> mrTitle),
          \pId mrIid mrTitle -> setMergeRequestTitle pId mrIid (strip $ fromMaybe mrTitle (stripPrefix "Draft:" mrTitle))
        )
      DryRun ->
        ( \_ _ -> Right () <$ write "dry run. skipping rebase",
          \_ _ _ -> Right () <$ write "dry run. skipping merge",
          \_ _ _ -> Right () <$ write "dry run. skipping merge attempt",
          \detailedStatus -> Right () <$ write ("The merge status is " <> show detailedStatus <> ", skipping the merge as it wouldn't succeed"),
          \_ _ _ -> Right () <$ write "dry run. skipping draft toggle",
          \_ _ _ -> Right () <$ write "dry run. skipping draft toggle"
        )

-- | Depending on the merge status of a merge request, trying a merge may or may not make sense.
-- There are cases where a merge will very likely be successful, others where it maybe will work, and others where it definitely won't work.
--
-- This is roughly equivalent to the question whether the UI says "merge", "set to auto-merge", or "a merge is not possible"
--
-- This type represents these different cases.
data MergeDecision = MergeShouldWork | MergeMayWork | MergeWontWork

detailedMergeStatusToDecision :: DetailedMergeStatus -> MergeDecision
detailedMergeStatusToDecision Mergeable = MergeShouldWork
detailedMergeStatusToDecision dms | dms `elem` [ApprovalsSyncing, Checking, CIStillRunning, Unchecked] = MergeMayWork
detailedMergeStatusToDecision _ = MergeWontWork
