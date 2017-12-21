{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromJust) -- TODO XXX fix this
import qualified Data.Text.Lazy as T
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Posix.Temp
import           System.Process
import           Text.ICalendar
import           Text.ICalendar.Cleaner


main :: IO ()
main = do
  tmpDir <- mkdtemp "/tmp/calendarmailer"
  let
    destDir = tmpDir </> cleaned
    newname = (destDir </>) . takeFileName
  createDirectory destDir
  getArgs >>= mapM_ ((p =<<) . cleanFile newname filterCalendar)
  callProcess "tar" ["cJf", tar, "-C", tmpDir, cleaned]
  removeDirectoryRecursive tmpDir
  where
    tar = "./cleaned-calendar.tar.xz"
    cleaned = "cleaned"
    p (Just a) = putStrLn a
    p Nothing  = return ()


filterCalendar :: [VCalendar] -> [VCalendar]
filterCalendar =
  L.filter (not . null . vcEvents)
  . L.map (\x -> x { vcEvents = filterEvents (vcEvents x) })


filterEvents :: M.Map (T.Text, Maybe (Either Date DateTime)) VEvent
             -> M.Map (T.Text, Maybe (Either Date DateTime)) VEvent
filterEvents = M.filter (("Lunch" /=) . summaryValue . fromJust . veSummary)
