module Main where

import Config
import Data.Configurator
import Github

main :: IO ()
main = getConfig >>= display
