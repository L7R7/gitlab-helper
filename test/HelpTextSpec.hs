{-# LANGUAGE TypeApplications #-}

module HelpTextSpec (spec) where

import Config.Optparse
import Options.Applicative
import Relude
import Test.Syd

spec :: Spec
spec = do
  it "provides a help text" $ do
    let res = execParserPure defaultPrefs (parserInfo) ["--help"]
    case res of
      Failure (ParserFailure f) -> do
        let (parserHelp, _, _) = f "gitlab-helper"
        goldenShowInstance "test_resources/help-text.txt" parserHelp
      _ -> undefined
