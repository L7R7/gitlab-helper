{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Projects
  ( listProjectsMeta,
    showProjectsForGroup,
    enableSourceBranchDeletionAfterMerge,
    enableSuccessfulPipelineForMergeRequirement,
    enableAllDiscussionsResolvedForMergeRequirement,
    setMergeMethodToFastForward,
    countDeployments,
  )
where

import App (App)
import Colonnade (asciiCapped, cap, headed)
import Colourista (bold, formatWith, red)
import Config
import Data.Aeson (encode)
import qualified Data.Map as M
import Data.Text (toLower)
import Effects
import Gitlab.Client.MTL (UpdateError)
import Gitlab.Group (Group)
import Gitlab.Lib (EnabledDisabled (..), Id (..), Name (..), Ref (..))
import Gitlab.Project
import Relude hiding (pi)

listProjectsMeta :: MetaScope -> App ()
listProjectsMeta MetaScopeAll = listAllProjectsMeta
listProjectsMeta MetaScopeGroup = listProjectsMetaForGroup

listAllProjectsMeta :: App ()
listAllProjectsMeta = fetch >>= bitraverse_ (write . show) writeMetaFormat
  where
    fetch =
      runExceptT
        $ (<>)
        <$> (getAllGroups' >>= fmap join . traverse getProjectsForGroup')
        <*> (getAllUsers' >>= fmap join . traverse (getProjectsForUser' . userId))

    getAllGroups' = ExceptT getAllGroups
    getProjectsForGroup' = ExceptT . const (getProjectsForGroup SkipArchivedProjects)
    getProjectsForUser' = ExceptT . getProjectsForUser
    getAllUsers' = ExceptT getAllUsers

showProjectsForGroup :: App ()
showProjectsForGroup = do
  gId <- asks groupId
  write "=================================================="
  write $ "Listing the projects for Group " <> show gId
  getProjectsForGroup SkipArchivedProjects >>= \case
    Left err -> write $ show err
    Right projects -> do
      write . toText $ tableReport (sortOn (toLower . getName . projectName) projects)
      writeSummary $ foldMap summarizeSingle projects

tableReport :: (Foldable f) => f Project -> String
tableReport =
  asciiCapped
    $ mconcat
      [ cap
          ""
          (headed "ID" (show . projectId) <> headed "Name" (show . projectName) <> headed "default branch" (maybe "-" (\(Ref r) -> toString r) . projectDefaultBranch)),
        cap
          "Merge Requests"
          (headed "enabled" (show . projectMergeRequestsEnabled) <> headed "Remove source branch after merge" (maybe "unknown" show . projectRemoveSourceBranchAfterMerge) <> headed "merge method" (show . projectMergeMethod)),
        cap
          "Requirements for a merge request to be merged"
          (headed "successful pipeline" (maybe "unknown" show . projectOnlyAllowMergeIfPipelineSucceeds) <> headed "all discussions resolved" (maybe "unknown" show . projectOnlyAllowMergeIfAllDiscussionsAreResolved)),
        cap
          "Pipelines"
          (headed "auto cancel pending" (show . projectAutoCancelPendingPipelines))
      ]

data Processor
  = OptionSetter
      -- | Include or skip archived projects
      WithArchivedProjects
      -- | headline to be printed
      (Id Group -> Text)
      -- | if this returns True, nothing will be done
      (Project -> Bool)
      -- | action to execute
      (Id Project -> App (Either UpdateError ()))
  | Counter
      -- | Include or skip archived projects
      WithArchivedProjects
      -- | headline to be printed
      (Id Group -> Text)
      -- | if this returns True, nothing will be done
      (Project -> Bool)
      -- | action to execute
      (Project -> App (Either UpdateError (Sum Int)))

countDeployments :: Year -> WithArchivedProjects -> App ()
countDeployments year@(Year y) withArchivedProjects = do
  excludes <- asks projectsExcludeList
  let archivedProjectsTitle = case withArchivedProjects of
        IncludeArchivedProjects -> "including"
        SkipArchivedProjects -> "excluding"
  runProcessor
    $ Counter
      withArchivedProjects
      (\gi -> "Listing the number of successful deployments in " <> show y <> " for all projects in group " <> show gi <> " " <> archivedProjectsTitle <> " archived projects")
      (\p -> projectId p `elem` excludes)
      ( \p -> do
          res <- case projectDefaultBranch p of
            Nothing -> Right [] <$ write (formatWith [bold] (getName (projectName p) <> ": ") <> formatWith [red] "has no default branch")
            Just ref -> getSuccessfulPushPipelines year (projectId p) ref
          pure $ fmap (Sum . length) res
      )

enableSourceBranchDeletionAfterMerge :: Execution -> App ()
enableSourceBranchDeletionAfterMerge execution =
  runProcessor
    $ OptionSetter
      SkipArchivedProjects
      (\gi -> "Enabling automatic branch deletion after MR merge for Group " <> show gi)
      (\p -> Just True == projectRemoveSourceBranchAfterMerge p)
      ( \pi -> case execution of
          DryRun -> Right () <$ write ("Dry Run. Pretending to set option for Project " <> show pi)
          Execute -> enableSourceBranchDeletionAfterMrMerge pi
      )

enableSuccessfulPipelineForMergeRequirement :: Execution -> App ()
enableSuccessfulPipelineForMergeRequirement execution =
  runProcessor
    $ OptionSetter
      SkipArchivedProjects
      (\gi -> "Enabling the requirement that a successful pipeline is required for a MR to be merged for Group " <> show gi)
      (or . projectOnlyAllowMergeIfPipelineSucceeds)
      (\pId -> getProject pId >>= (projectHasCi >=> configureOption execution pId))

projectHasCi :: Either UpdateError Project -> App (Either UpdateError Bool)
projectHasCi (Left err) = pure $ Left err
projectHasCi (Right (Project pId _ _ (Just ref) _ _ _ _ _ _ _ _ _ _ _)) = hasCi pId ref
projectHasCi (Right _) = pure $ Right False -- no default branch, no CI

configureOption :: Execution -> Id Project -> Either UpdateError Bool -> App (Either UpdateError ())
configureOption _ _ (Left err) = pure $ Left err
configureOption DryRun _ (Right False) = Right () <$ write "Dry Run. Pretending to unset option for project" >> logUnset
configureOption Execute pId (Right False) = unsetSuccessfulPipelineRequirementForMerge pId >> logUnset
configureOption DryRun _ (Right True) = Right () <$ write "Dry Run. Pretending to set option for project"
configureOption Execute pId (Right True) = setSuccessfulPipelineRequirementForMerge pId

logUnset :: App (Either UpdateError ())
logUnset = write "Project doesn't have CI. Deactivated the option." $> Right ()

enableAllDiscussionsResolvedForMergeRequirement :: Execution -> App ()
enableAllDiscussionsResolvedForMergeRequirement execution =
  runProcessor
    $ OptionSetter
      SkipArchivedProjects
      (\gi -> "Enabling the requirement that all discussions must be resolved for a MR to be merged for Group " <> show gi)
      (or . projectOnlyAllowMergeIfAllDiscussionsAreResolved)
      ( case execution of
          DryRun -> (\pId -> Right () <$ write ("Dry Run. Pretending to set option for Project " <> show pId))
          Execute -> setResolvedDiscussionsRequirementForMerge
      )

setMergeMethodToFastForward :: Execution -> App ()
setMergeMethodToFastForward execution =
  runProcessor
    $ OptionSetter
      SkipArchivedProjects
      (\gi -> "Setting the merge method to \"Fast Forward\" for all projects in group " <> show gi)
      (\p -> projectMergeMethod p == FastForward)
      ( case execution of
          DryRun -> (\pId -> Right () <$ write ("Dry Run. Pretending to set merge method for Project " <> show pId))
          Execute -> (`setMergeMethod` FastForward)
      )

listProjectsMetaForGroup :: App ()
listProjectsMetaForGroup = do
  getProjectsForGroup SkipArchivedProjects >>= \case
    Left err -> write $ show err
    Right projects -> writeMetaFormat projects

writeMetaFormat :: [Project] -> App ()
writeMetaFormat projects = write $ decodeUtf8 $ encode $ M.fromList $ (\p -> (projectPathWithNamespace p, projectSshUrlToRepo p)) <$> projects

runProcessor :: Processor -> App ()
runProcessor (OptionSetter withArchivedProjects title skipIf action) = do
  gId <- asks groupId
  write "=================================================="
  write $ title gId
  getProjectsForGroup withArchivedProjects >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse (process skipIf action) projects
      write ""
      write "done: "
      let summary = foldl' (\m r -> M.insertWith (<>) r (Sum (1 :: Int)) m) (M.fromList $ (,mempty) <$> universe) res
      let summaryPrint = M.foldlWithKey' (\acc k (Sum c) -> (show k <> ": " <> show c) : acc) mempty summary
      traverse_ write summaryPrint
runProcessor (Counter withArchivedProjects title skipIf action) = do
  gId <- asks groupId
  write "=================================================="
  write $ title gId
  write ""
  getProjectsForGroup withArchivedProjects >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse (countSingle skipIf action) projects
      write ""
      write $ "done. Total: " <> show (getSum $ fold res) <> " deployments"

process :: (Project -> Bool) -> (Id Project -> App (Either UpdateError ())) -> Project -> App Result
process skipIf action project = do
  write ""
  write $ formatWith [bold] ("=== " <> show (projectName project))
  if skipIf project
    then write "option is already enabled. Not doing anything" $> AlreadySet
    else do
      write "setting option"
      res <- action (projectId project)
      case res of
        Left err -> write ("something went wrong. " <> show err) $> Error
        Right _ -> write "done" $> Set

countSingle :: (Project -> Bool) -> (Project -> App (Either UpdateError (Sum Int))) -> Project -> App (Sum Int)
countSingle skipIf action project = count >>= \(output, result) -> write (title <> output) $> result
  where
    count =
      if skipIf project
        then pure ("skipped", mempty)
        else do
          res <- action project
          case res of
            Left err -> pure (formatWith [red] "something went wrong: " <> show err, mempty)
            Right s -> pure (show (getSum s) <> " deployments", s)
    title = formatWith [bold] (show (projectName project) <> " (#" <> show (projectId project) <> "): ")

data Result = AlreadySet | Set | Error deriving stock (Bounded, Enum, Eq, Ord, Show)

newtype Count a = Count Int
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

newtype EnabledDisabledCount a = EnabledDisabledCount (Count a, Count a)
  deriving newtype (Semigroup)
  deriving newtype (Monoid)

mkEnabledDisabledCount :: EnabledDisabled -> EnabledDisabledCount a
mkEnabledDisabledCount Enabled = EnabledDisabledCount (Count 1, Count 0)
mkEnabledDisabledCount Disabled = EnabledDisabledCount (Count 0, Count 1)

mkEnabledDisabledCount' :: Bool -> EnabledDisabledCount a
mkEnabledDisabledCount' b = if b then EnabledDisabledCount (Count 1, Count 0) else EnabledDisabledCount (Count 0, Count 1)

data ProjectCount

data SourceBranchDeletion

data HasNoDefaultBranch

data SuccessfulPipelineForMerge

data AllDiscussionsResolvedForMerge

data AutCancelRedundantPipelines

type Summary =
  ( Count ProjectCount,
    EnabledDisabledCount SourceBranchDeletion,
    Count HasNoDefaultBranch,
    EnabledDisabledCount SuccessfulPipelineForMerge,
    ( EnabledDisabledCount AllDiscussionsResolvedForMerge,
      EnabledDisabledCount AutCancelRedundantPipelines
    )
  )

writeSummary :: Summary -> App ()
writeSummary
  ( Count numProjects,
    EnabledDisabledCount (Count branchDeletionEnabled, Count branchDeletionDisabled),
    Count hasNoDefaultBranch,
    EnabledDisabledCount (Count successfulPipelineForMergeEnabled, Count successfulPipelineForMergeDisabled),
    ( EnabledDisabledCount (Count allDiscussionsResolvedForMergeEnabled, Count allDiscussionsResolvedForMergeDisabled),
      EnabledDisabledCount (Count autoCancelPendingPipelinesEnabled, Count autoCancelPendingPipelinesDisabled)
      )
    ) = do
    write $ formatWith [bold] "=== Summary"
    write $ "Anzahl Projekte: " <> show numProjects
    write $ "Projekte die 'remove_source_branch_after_merge' aktiviert haben: " <> show branchDeletionEnabled
    write $ "             'remove_source_branch_after_merge' NICHT aktiviert haben: " <> show branchDeletionDisabled
    write $ "Projekte die 'only_allow_merge_if_pipeline_succeeds' aktiviert haben: " <> show successfulPipelineForMergeEnabled
    write $ "             'only_allow_merge_if_pipeline_succeeds' NICHT aktiviert haben: " <> show successfulPipelineForMergeDisabled
    write $ "Projekte die 'only_allow_merge_if_all_discussions_are_resolved' aktiviert haben: " <> show allDiscussionsResolvedForMergeEnabled
    write $ "             'only_allow_merge_if_all_discussions_are_resolved' NICHT aktiviert haben: " <> show allDiscussionsResolvedForMergeDisabled
    write $ "Projekte die 'auto_cancel_pending_pipelines' aktiviert haben: " <> show autoCancelPendingPipelinesEnabled
    write $ "             'auto_cancel_pending_pipelines' NICHT aktiviert haben: " <> show autoCancelPendingPipelinesDisabled
    write $ "Projekte ohne default branch: " <> show hasNoDefaultBranch

summarizeSingle :: Project -> Summary
summarizeSingle project =
  ( Count 1,
    sourceBranchDeletionEnabledDisabled,
    noDefaultBranch,
    successfulPipelineForMergeEnabledDisabled,
    ( allDiscussionsResolvedForMergeEnabledDisabled,
      autoCancelPendingPipelinesEnabledDisabled
    )
  )
  where
    sourceBranchDeletionEnabledDisabled = mkEnabledDisabledCount' branchDeletionEnabled
    branchDeletionEnabled = or (projectRemoveSourceBranchAfterMerge project)
    noDefaultBranch = Count $ if isJust (projectDefaultBranch project) then 0 else 1
    successfulPipelineForMergeEnabledDisabled = mkEnabledDisabledCount' $ or $ projectOnlyAllowMergeIfPipelineSucceeds project
    allDiscussionsResolvedForMergeEnabledDisabled = mkEnabledDisabledCount' $ or $ projectOnlyAllowMergeIfAllDiscussionsAreResolved project
    autoCancelPendingPipelinesEnabledDisabled = mkEnabledDisabledCount (fromMaybe Disabled (projectAutoCancelPendingPipelines project))
