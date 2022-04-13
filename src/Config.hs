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

getIssueQuery :: Text -> IO String
getIssueQuery x = getConfig >>= (`require` x) :: IO String
