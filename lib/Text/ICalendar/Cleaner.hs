module Text.ICalendar.Cleaner (cleanFile) where

import           Prelude hiding (concat, read, readFile, writeFile)
import           Data.ByteString.Lazy (concat, readFile, writeFile)
import           Data.Default
import           Data.Map (fromList, elems, keys)
import qualified Data.Set as S
import           Data.Text.Lazy (pack)
import           Text.ICalendar
import           Text.ICalendar.Sanitizer


-- |Create a cleaned copy of 'FilePath'
cleanFile :: (FilePath -> FilePath) -- ^ Function to create an FilePath to which the cleaned version will be written
          -> FilePath               -- ^ FilePath to the calendar that shall be cleaned
          -> IO ()
cleanFile newname f =
  read f >>= clean >>= write (newname f)
  where
    un :: Either String ([VCalendar], [String]) -> [VCalendar]
    un (Left  a)      = error a
    un (Right (a, _)) = a
    read :: FilePath -> IO [VCalendar]
    read c = un <$> parseICalendar def c <$> sanitize c <$> readFile c
    write :: FilePath -> [VCalendar] -> IO ()
    write c = writeFile c . concat . map (printICalendar def)
    clean :: Monad m => [VCalendar] -> m [VCalendar]
    clean = mapM $ return . cleanCalendar


cleanCalendar :: VCalendar -> VCalendar
cleanCalendar c =
  c { vcEvents = fromList (zip k events) }
  where
    events = map cleanEvent $ elems $ vcEvents c
    k = keys $ vcEvents c


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
    fixTZ (ExDateTimes ts o) = ExDateTimes (S.map fixTZ' ts) o
    fixTZ a                  = a
    fixTZ' (ZonedDateTime dt tz) = if tz == pack "W. Europe Standard Time"
                                     then ZonedDateTime dt $ pack "Europe/Berlin"
                                     else ZonedDateTime dt tz
    fixTZ' a                     = a
