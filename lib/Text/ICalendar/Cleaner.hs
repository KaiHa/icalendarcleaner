{-# LANGUAGE OverloadedStrings #-}
module Text.ICalendar.Cleaner (cleanFile) where

import           Prelude hiding (concat, read, readFile, writeFile)
import           Data.ByteString.Lazy (concat, readFile, writeFile)
import           Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.ICalendar
import           Text.ICalendar.Sanitizer


-- |Create a cleaned copy of 'FilePath'
cleanFile :: (FilePath -> FilePath) -- ^ Function to create an FilePath to which the cleaned version will be written
          -> FilePath               -- ^ FilePath to the calendar that shall be cleaned
          -> IO (Maybe String)
cleanFile newname f =
  read f >>= \c -> case c of
                     (Left a)       -> return $ Just a
                     (Right (a, _)) -> clean a >>= write (newname f) >> return Nothing
  where
    read :: FilePath -> IO (Either String ([VCalendar], [String]))
    read c = parseICalendar def c <$> sanitize c <$> readFile c
    write :: FilePath -> [VCalendar] -> IO ()
    write c = writeFile c . concat . map (printICalendar def)
    clean :: Monad m => [VCalendar] -> m [VCalendar]
    clean = mapM $ return . cleanCalendar


cleanCalendar :: VCalendar -> VCalendar
cleanCalendar c =
  c { vcEvents = M.fromList (zip k events) }
  where
    events = map cleanEvent $ M.elems $ vcEvents c
    k = M.keys $ vcEvents c


cleanEvent :: VEvent -> VEvent
cleanEvent e = VEvent
  (veDTStamp e)
  (veUID e)
  (veClass e)
  (veDTStart e)
  (veCreated e)
  Nothing -- veDescription
  Nothing -- veGeo
  Nothing -- veLastMod
  (veLocation e)
  Nothing -- veOrganizer
  def -- vePriority
  def -- veSeq
  Nothing -- veStatus
  (veSummary e)
  def -- veTransp
  Nothing -- veUrl
  (veRecurId e)
  (veRRule e)
  (veDTEndDuration e)
  S.empty -- veAttach
  S.empty -- veAttendee
  S.empty -- veCategories
  S.empty -- veComment
  S.empty -- veContact
  (myExDate e)
  S.empty -- veRStatus
  S.empty -- veRelated
  S.empty -- veResources
  (veRDate e)
  S.empty -- veAlarms
  S.empty -- veOther
  where
    myExDate = S.map fixTZ . veExDate
    fixTZ a@(ExDateTimes b _) = a { exDateTimes = S.map fixTZ' b }
    fixTZ a                   = a
    fixTZ' a@(ZonedDateTime _ tz) = if tz == "W. Europe Standard Time"
                                    then a { dateTimeZone = "Europe/Berlin" }
                                    else a
    fixTZ' a                      = a
