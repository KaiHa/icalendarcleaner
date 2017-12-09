module Main (main) where

import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Posix.Temp
import           System.Process
import           Text.ICalendar.Cleaner


main :: IO ()
main = do
  tmpDir <- mkdtemp "calendarmailer"
  let
    destDir = tmpDir </> cleaned
    newname = (destDir </>) . takeFileName
  createDirectory destDir
  getArgs >>= mapM_ ((p =<<) . cleanFile newname)
  callProcess "tar" ["cJf", tar, "-C", tmpDir, cleaned]
  removeDirectoryRecursive tmpDir
  where
    tar = "./cleaned-calendar.tar.xz"
    cleaned = "cleaned"
    p (Just a) = putStrLn a
    p Nothing  = return ()
