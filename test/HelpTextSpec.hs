{-# OPTIONS_GHC -Wno-deprecations #-}

module HelpTextSpec (spec) where

import Config.Optparse
import Options.Applicative
import Relude
import Test.Syd

spec :: Spec
spec = do
  it "provides a help text for the main program" $ do
    let res = execParserPure defaultPrefs parserInfo ["--help"]
    case res of
      Failure (ParserFailure f) -> do
        let (parserHelp, _, _) = f "gitlab-helper"
        goldenShowInstance "test_resources/help-text.txt" parserHelp
      _ -> undefined
  it "provides a help text for working with MRs" $ do
    let res = execParserPure defaultPrefs parserInfo ["update-merge-requests", "--help"]
    case res of
      Failure (ParserFailure f) -> do
        let (parserHelp, _, _) = f "gitlab-helper"
        goldenShowInstance "test_resources/help-text-mrs.txt" parserHelp
      _ -> undefined
