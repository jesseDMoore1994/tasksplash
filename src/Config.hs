{-# LANGUAGE OverloadedStrings #-}

module Config
  ( getConfig
  , getIssueQuery
  ) where

import Data.Configurator
import Data.Configurator.Types

getConfig :: IO Config
getConfig = load [Required "$(HOME)/.tasksplash.cfg"]

getIssueQuery :: IO String
getIssueQuery = getConfig >>= flip require "IssueQuery"
