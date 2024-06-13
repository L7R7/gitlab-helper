{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Projects
  ( listAllProjectsMeta,
    showProjectsForGroup,
    enableSourceBranchDeletionAfterMerge,
    enableSuccessfulPipelineForMergeRequirement,
    enableAllDiscussionsResolvedForMergeRequirement,
    listProjectsMetaForGroup,
    countDeploymentsIn2022,
  )
where

import Colonnade
import Colourista (bold, formatWith, red)
import Config.Types
import Data.Aeson (encode)
import qualified Data.Map as M
import Data.Text (toLower)
import Effects
import qualified Effects as G
import Polysemy
import Relude hiding (pi)

listAllProjectsMeta :: (Member UsersApi r, Member GroupsApi r, Member ProjectsApi r, Member Writer r) => Sem r ()
listAllProjectsMeta = fetch >>= bitraverse_ (write . show) writeMetaFormat
  where
    fetch =
      runExceptT $
        (<>)
          <$> (getAllGroups' >>= fmap join . traverse (getProjectsForGroup' . G.groupId))
          <*> (getAllUsers' >>= fmap join . traverse (getProjectsForUser' . userId))

    getAllGroups' = ExceptT getAllGroups
    getProjectsForGroup' = ExceptT . getProjectsForGroup
    getProjectsForUser' = ExceptT . getProjectsForUser
    getAllUsers' = ExceptT getAllUsers

showProjectsForGroup :: (Member ProjectsApi r, Member Writer r) => GroupId -> Sem r ()
showProjectsForGroup gId = do
  write "=================================================="
  write $ "Listing the projects for Group " <> show gId
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      write . toText $ tableReport (sortOn (toLower . getProjectName . name) projects)
      writeSummary $ foldMap summarizeSingle projects

tableReport :: Foldable f => f Project -> String
tableReport =
  asciiCapped $
    mconcat
      [ cap
          ""
          (headed "ID" (show . projectId) <> headed "Name" (show . name) <> headed "default branch" (maybe "-" (\(Ref r) -> toString r) . defaultBranch)),
        cap
          "Merge Requests"
          (headed "enabled" (show . mergeRequestsEnabled) <> headed "Remove source branch after merge" (maybe "unknown" show . removeSourceBranchAfterMerge) <> headed "merge method" (show . mergeMethod)),
        cap
          "Requirements for a merge request to be merged"
          (headed "successful pipeline" (maybe "unknown" show . onlyAllowMergeIfPipelineSucceeds) <> headed "all discussions resolved" (maybe "unknown" show . onlyAllowMergeIfAllDiscussionsAreResolved)),
        cap
          "Pipelines"
          (headed "auto cancel pending" (show . autoCancelPendingPipelines))
      ]

data Processor r
  = OptionSetter
      GroupId
      (GroupId -> Text)
      -- ^ headline to be printed
      (Project -> Bool)
      -- ^ if this returns True, nothing will be done
      (ProjectId -> Sem r (Either UpdateError ()))
      -- ^ action to execute
  | Counter
      GroupId
      (GroupId -> Text)
      -- ^ headline to be printed
      (Project -> Bool)
      -- ^ if this returns True, nothing will be done
      (Project -> Sem r (Either UpdateError (Sum Int)))
      -- ^ action to execute

countDeploymentsIn2022 :: (Member ProjectsApi r, Member PipelinesApi r, Member Writer r) => GroupId -> Sem r ()
countDeploymentsIn2022 gId =
  runProcessor $
    Counter
      gId
      (\gi -> "Listing the number of successful deployments in 2022 for all projects in Group " <> show gi)
      (\p -> projectId p `elem` excludes)
      ( \p -> do
          res <- case (defaultBranch p) of
            Nothing -> Right [] <$ (write $ formatWith [bold] (show (name p) <> ": ") <> formatWith [red] "has no default branch")
            Just ref -> getSuccessfulPushPipelinesIn2022 (projectId p) ref
          pure $ fmap (Sum . length) res
      )
  where
    excludes =
      ProjectId
        <$> [94, 198, 509, 554, 572, 583, 587, 648, 663, 738]
          ++ [1071, 1141, 1703, 1720, 1726, 1757, 1759, 1765, 1766, 1767, 1770, 1771, 1871]
          ++ [2011, 2040, 2041, 2145, 2151, 2155, 2191, 2260, 2261, 2328, 2507, 2756, 2828, 2843, 2871, 2941]
          ++ [3015, 3053]

enableSourceBranchDeletionAfterMerge :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => Execution -> GroupId -> Sem r ()
enableSourceBranchDeletionAfterMerge execution gId =
  runProcessor $
    OptionSetter
      gId
      (\gi -> "Enabling automatic branch deletion after MR merge for Group " <> show gi)
      (\p -> Just True == removeSourceBranchAfterMerge p)
      ( \pi -> case execution of
          DryRun -> Right () <$ write ("Dry Run. Pretending to set option for Project " <> show pi)
          Execute -> enableSourceBranchDeletionAfterMrMerge pi
      )

enableSuccessfulPipelineForMergeRequirement :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => Execution -> GroupId -> Sem r ()
enableSuccessfulPipelineForMergeRequirement execution gId =
  runProcessor $
    OptionSetter
      gId
      (\gi -> "Enabling the requirement that a successful pipeline is required for a MR to be merged for Group " <> show gi)
      (or . onlyAllowMergeIfPipelineSucceeds)
      (\pId -> getProject pId >>= (projectHasCi >=> configureOption execution pId))

projectHasCi :: (Member ProjectsApi r) => Either UpdateError Project -> Sem r (Either UpdateError Bool)
projectHasCi (Left err) = pure $ Left err
projectHasCi (Right (Project pId _ _ _ (Just ref) _ _ _ _ _ _)) = hasCi pId ref
projectHasCi (Right _) = pure $ Right False -- no default branch, no CI

configureOption :: (Member MergeRequestApi r, Member Writer r) => Execution -> ProjectId -> Either UpdateError Bool -> Sem r (Either UpdateError ())
configureOption _ _ (Left err) = pure $ Left err
configureOption DryRun _ (Right False) = Right () <$ write "Dry Run. Pretending to unset option for project" >> logUnset
configureOption Execute pId (Right False) = unsetSuccessfulPipelineRequirementForMerge pId >> logUnset
configureOption DryRun _ (Right True) = Right () <$ write "Dry Run. Pretending to set option for project"
configureOption Execute pId (Right True) = setSuccessfulPipelineRequirementForMerge pId

logUnset :: Member Writer r => Sem r (Either UpdateError ())
logUnset = write "Project doesn't have CI. Deactivated the option." $> Right ()

enableAllDiscussionsResolvedForMergeRequirement :: (Member ProjectsApi r, Member MergeRequestApi r, Member Writer r) => Execution -> GroupId -> Sem r ()
enableAllDiscussionsResolvedForMergeRequirement execution gId =
  runProcessor $
    OptionSetter
      gId
      (\gi -> "Enabling the requirement that all discussions must be resolved for a MR to be merged for Group " <> show gi)
      (or . onlyAllowMergeIfAllDiscussionsAreResolved)
      ( case execution of
          DryRun -> (\pId -> Right () <$ write ("Dry Run. Pretending to set option for Project " <> show pId))
          Execute -> setResolvedDiscussionsRequirementForMerge
      )

listProjectsMetaForGroup :: (Member ProjectsApi r, Member Writer r) => GroupId -> Sem r ()
listProjectsMetaForGroup gId =
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> writeMetaFormat projects

writeMetaFormat :: (Member Writer r) => [Project] -> Sem r ()
writeMetaFormat projects = write $ decodeUtf8 $ encode $ M.fromList $ (\p -> (pathWithNamespace p, sshUrlToRepo p)) <$> projects

runProcessor :: (Member ProjectsApi r, Member Writer r) => Processor r -> Sem r ()
runProcessor (OptionSetter gId title skipIf action) = do
  write "=================================================="
  write $ title gId
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse (process skipIf action) projects
      write ""
      write "done: "
      let summary = foldl' (\m r -> M.insertWith (<>) r (Sum (1 :: Int)) m) (M.fromList $ (,mempty) <$> universe) res
      let summaryPrint = M.foldlWithKey' (\acc k (Sum c) -> (show k <> ": " <> show c) : acc) mempty summary
      traverse_ write summaryPrint
runProcessor (Counter gId title skipIf action) = do
  write "=================================================="
  write $ title gId
  write ""
  getProjectsForGroup gId >>= \case
    Left err -> write $ show err
    Right projects -> do
      res <- traverse (countSingle skipIf action) projects
      write ""
      write $ "done. in total: " <> show (getSum $ fold res) <> " deployments"

process :: (Member Writer r) => (Project -> Bool) -> (ProjectId -> Sem r (Either UpdateError ())) -> Project -> Sem r Result
process skipIf action project = do
  write ""
  write $ formatWith [bold] ("=== " <> show (name project))
  if skipIf project
    then write "option is already enabled. Not doing anything" $> AlreadySet
    else do
      write "setting option"
      res <- action (projectId project)
      case res of
        Left err -> write ("something went wrong. " <> show err) $> Error
        Right _ -> write "done" $> Set

countSingle :: (Member Writer r) => (Project -> Bool) -> (Project -> Sem r (Either UpdateError (Sum Int))) -> Project -> Sem r (Sum Int)
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
    title = formatWith [bold] (show (name project) <> ": ") -- "(" <> show (projectId project) <> "): ")

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

writeSummary :: (Member Writer r) => Summary -> Sem r ()
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
    branchDeletionEnabled = or (removeSourceBranchAfterMerge project)
    noDefaultBranch = Count $ if isJust (defaultBranch project) then 0 else 1
    successfulPipelineForMergeEnabledDisabled = mkEnabledDisabledCount' $ or $ onlyAllowMergeIfPipelineSucceeds project
    allDiscussionsResolvedForMergeEnabledDisabled = mkEnabledDisabledCount' $ or $ onlyAllowMergeIfAllDiscussionsAreResolved project
    autoCancelPendingPipelinesEnabledDisabled = mkEnabledDisabledCount (autoCancelPendingPipelines project)
