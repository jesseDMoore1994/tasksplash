{-# LANGUAGE OverloadedStrings #-}

module Config
  ( getConfig
  , getIssueQuery
  ) where

import Data.Configurator
import Data.Configurator.Types
import Data.Text

getConfig :: IO Config
getConfig = load [Required "$(HOME)/.tasksplash.cfg"]

getIssueQuery :: Config -> IO String
getIssueQuery config = (config `require` "IssueQuery") :: IO String
