{-# LANGUAGE OverloadedStrings #-}
module Text.ICalendar.Cleaner (cleanFile) where

import           Prelude hiding (concat, read, readFile, writeFile)
import           Data.ByteString.Lazy (concat, readFile, writeFile)
import           Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
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
  c { vcEvents    = M.map cleanEvent $ vcEvents c
    , vcTimeZones = M.map tzfix $ vcTimeZones c
    }
  where
    tzfix a = a { vtzId = fixTZ $ vtzId a }


cleanEvent :: VEvent -> VEvent
cleanEvent e = VEvent
  (veDTStamp e)
  (veUID e)
  (veClass e)
  (fixTZ <$> veDTStart e)
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
  (fixTZ <$> veDTEndDuration e)
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
    myExDate = S.map fixTZ' . veExDate
    fixTZ' a@(ExDateTimes b _) = a { exDateTimes = S.map fixTZ b }
    fixTZ' a                   = a


instance FixTZ T.Text where
  fixTZ a = if a == "W. Europe Standard Time"
               || "(UTC+01:00) Amsterdam" `T.isPrefixOf` a
            then "Europe/Berlin"
            else a

instance FixTZ DateTime where
  fixTZ a@(ZonedDateTime _ b) = a { dateTimeZone = fixTZ b }
  fixTZ a                     = a

instance FixTZ a => FixTZ (Either a b) where
  fixTZ (Left a) = Left $ fixTZ a
  fixTZ a        = a

instance FixTZ DTStart where
  fixTZ a@(DTStartDateTime b _) = a { dtStartDateTimeValue = fixTZ b }
  fixTZ a                       = a

instance FixTZ DTEnd where
  fixTZ a@(DTEndDateTime b _) = a { dtEndDateTimeValue = fixTZ b }
  fixTZ a                       = a

instance FixTZ TZID where
  fixTZ a@(TZID b _ _) = a { tzidValue = fixTZ b }

class FixTZ a where
  fixTZ :: a -> a
