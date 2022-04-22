module Main where

import Config
import Data.Configurator
import Github
import UI

main :: IO ()
main = getConfig >>= run
