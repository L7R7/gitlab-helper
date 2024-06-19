{-# OPTIONS_GHC -Wno-deprecations #-}

module HelpTextSpec (spec) where

import Config.Optparse
import Options.Applicative
import Relude
import Test.Syd

spec :: Spec
spec = do
  it "provides a help text for the main program" $ parserGoldenTest ["--help"] "test_resources/help-text.txt"
  it "provides a help text for working with MRs" $ parserGoldenTest ["update-merge-requests", "--help"] "test_resources/help-text-mrs.txt"
  it "returns a useful output when no command is provided" $ parserGoldenTest [] "test_resources/help-text.txt"

parserGoldenTest :: [String] -> FilePath -> GoldenTest String
parserGoldenTest args fp = do
  let res = execParserPure parserPrefs parserInfo args
  case res of
    Failure (ParserFailure f) -> do
      let (parserHelp, _, _) = f "gitlab-helper"
      goldenShowInstance fp parserHelp
    _ -> undefined
