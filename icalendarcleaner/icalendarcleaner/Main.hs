module Main (main) where

import System.Environment
import Text.ICalendar.Cleaner


main :: IO ()
main = getArgs >>= mapM_ ((p =<<) . cleanFile (++ ".cleaned") id)
  where
    p (Just a) = putStrLn a
    p Nothing  = return ()
