module Main (main) where

import Prelude hiding (concat, readFile, writeFile)
import Data.ByteString.Lazy (concat, readFile, writeFile)
import Data.Default
import Data.Map (fromList, elems, keys)
import Data.Maybe
import Data.Set (empty)
import System.Environment
import Text.ICalendar


main :: IO ()
main = getArgs >>= mapM_ cleanFile


-- |Create a cleaned copy of 'FilePath'
cleanFile :: FilePath -> IO ()
cleanFile f =
  read f >>= clean >>= write (f ++ ".cleaned")
  where
    un :: Either String ([VCalendar], [String]) -> [VCalendar]
    un (Left  a)      = error a
    un (Right (a, _)) = a
    read :: FilePath -> IO [VCalendar]
    read f = un <$> parseICalendar def f <$> readFile f
    write :: FilePath -> [VCalendar] -> IO ()
    write f = writeFile f . concat . map (printICalendar def)
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
  empty -- veAttach
  empty -- veAttendee
  empty -- veCategories
  empty -- veComment
  empty -- veContact
  (veExDate e)
  empty -- veRStatus
  empty -- veRelated
  empty -- veResources
  (veRDate e)
  empty -- veAlarms
  empty -- veOther
