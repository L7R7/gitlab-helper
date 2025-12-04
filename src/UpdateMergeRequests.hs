{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module UpdateMergeRequests
  ( updateMergeRequests,
  )
where

import App (App)
import Config.Types
import Data.Text (isInfixOf, strip, stripPrefix, toLower)
import Effects
import Gitlab.Client.Queue.MTL (ProcessResult (..), UpdateError)
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
  res <- getOpenMergeRequestsForGroupQueued authorIs searchTerm' recheckMergeStatus $ \mr -> do
    if titleFilter mr && excludeFilter mr
      then do
        (txt, res) <- performAction mr
        pure $ PrintLinesWithResult (mconcat (mrTextLine mr <> maybe [] (\t -> [" >> ", t]) txt) :| []) () <$ res
      else pure $ Right Empty
  case res of
    Left err -> write $ "failed to update merge requests: " <> show err
    Right [] -> write "no merge requests to process"
    Right updates -> write $ show (length updates) <> " merge requests"
  where
    mrTextLine mr =
      [ "#",
        show (mergeRequestIid mr),
        " in Project #",
        show (mergeRequestProjectId mr),
        " with state ",
        show (mergeRequestDetailedMergeStatus mr),
        ": ",
        mergeRequestTitle mr
      ]
    titleFilter mr = case maybeSearchTerms of
      Just (Right (SearchTermTitle s)) -> toLower (toText s) `isInfixOf` toLower (mergeRequestTitle mr)
      _ -> True
    excludeFilter mr = mergeRequestProjectId mr `notElem` projectExcludes
    performAction :: MergeRequest -> App (Maybe Text, Either UpdateError ())
    performAction mr =
      let pId = mergeRequestProjectId mr
       in case action of
            List -> pure (Nothing, Right ())
            Rebase -> rebaseAction pId (mergeRequestIid mr)
            (Merge mergeCiOption) -> case detailedMergeStatusToDecision (mergeRequestDetailedMergeStatus mr) of
              MergeShouldWork -> mergeAction pId (mergeRequestIid mr) mergeCiOption
              MergeMayWork -> mergeAttemptAction pId (mergeRequestIid mr) mergeCiOption
              MergeWontWork -> mergeWontWorkAction (mergeRequestDetailedMergeStatus mr)
            SetToDraft ->
              if mergeRequestWip mr
                then pure (Just "merge request is already in state \"Draft\"", Right ())
                else setToDraftAction pId (mergeRequestIid mr) (mergeRequestTitle mr)
            MarkAsReady ->
              if mergeRequestWip mr
                then markAsReadyAction pId (mergeRequestIid mr) (mergeRequestTitle mr)
                else pure (Just "merge request is already marked as ready", Right ())

    (rebaseAction, mergeAction, mergeAttemptAction, mergeWontWorkAction, setToDraftAction, markAsReadyAction) = case execute of
      Execute ->
        ( \pId mId -> (Nothing,) <$> rebaseMergeRequest pId mId,
          \pId mId mco -> (Nothing,) <$> mergeMergeRequest pId mId mco,
          \pId mId mco -> (Nothing,) <$> mergeMergeRequest pId mId mco,
          \detailedStatus -> pure (Just $ "The merge status is " <> show detailedStatus <> ", skipping the merge as it wouldn't succeed", Right ()),
          \pId mrIid mrTitle -> (Nothing,) <$> setMergeRequestTitle pId mrIid ("Draft: " <> mrTitle),
          \pId mrIid mrTitle -> (Nothing,) <$> setMergeRequestTitle pId mrIid (strip $ fromMaybe mrTitle (stripPrefix "Draft:" mrTitle))
        )
      DryRun ->
        ( \_ _ -> pure (Just "dry run. skipping rebase", Right ()),
          \_ _ _ -> pure (Just "dry run. skipping merge", Right ()),
          \_ _ _ -> pure (Just "dry run. skipping merge attempt", Right ()),
          \detailedStatus -> pure (Just $ "The merge status is " <> show detailedStatus <> ", skipping the merge as it wouldn't succeed", Right ()),
          \_ _ _ -> pure (Just "dry run. skipping draft toggle", Right ()),
          \_ _ _ -> pure (Just "dry run. skipping draft toggle", Right ())
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
