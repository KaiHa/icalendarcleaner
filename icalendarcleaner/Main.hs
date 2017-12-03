module Main (main) where

import System.Environment
import Text.ICalendar.Cleaner


main :: IO ()
main = getArgs >>= mapM_ cleanFile
