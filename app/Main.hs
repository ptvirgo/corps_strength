module Main where

import Control.Monad
import Core
import Exercises


main :: IO ()
main = (makeMission $ Just Kettlebell) >>= mapM_ print
