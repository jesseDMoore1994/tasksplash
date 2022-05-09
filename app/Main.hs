module Main where

import Tasksplash

main :: IO ()
main = getConfig >>= run
