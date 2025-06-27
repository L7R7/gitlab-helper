{-# LANGUAGE TypeApplications #-}

module HelpTextSpec (spec) where

import Config
import OptEnvConf.Test
-- import Relude
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Config
  goldenSettingsReferenceDocumentationSpec @Config "test_resources/reference.txt" "gitlab-helper"
