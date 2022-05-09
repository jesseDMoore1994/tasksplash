{-# LANGUAGE OverloadedStrings #-}

module Config.Config
  ( getConfig
  ) where

import Data.Configurator
import Data.Configurator.Types

getConfig :: IO Config
getConfig = load [Required "$(HOME)/.tasksplash.cfg"]
