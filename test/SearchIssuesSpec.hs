{-# LANGUAGE OverloadedStrings #-}

module SearchIssuesSpec
  ( tests
  ) where

import Config.Config
import Github.SearchIssues
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

readToken :: Property
readToken = property $ do True === True

tests :: IO Bool
tests = checkSequential $ Group "SearchIssuesSpec" [("readToken", readToken)]
