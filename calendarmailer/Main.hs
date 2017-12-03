module Main (main) where

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy   as BS
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Posix.Temp
import           Text.ICalendar.Cleaner


main :: IO ()
main = do
  tmpDir <- mkdtemp "calendarmailer"
  let
    destDir = tmpDir </> cleaned
    newname = (destDir </>) . takeFileName
  createDirectory destDir
  getArgs >>= mapM_ (cleanFile newname)
  BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack tmpDir [cleaned]
  removeDirectoryRecursive tmpDir
  where
    tar = "./cleaned-calendar.tar.gz"
    cleaned = "cleaned"
