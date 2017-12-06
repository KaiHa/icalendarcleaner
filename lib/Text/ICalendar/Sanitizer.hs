module Text.ICalendar.Sanitizer (sanitize) where

import qualified Data.ByteString.Lazy as BS
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.List (intercalate)
import           Data.List.Extra (chunksOf)
import           Text.Parsec

sanitize :: SourceName -> BS.ByteString -> BS.ByteString
sanitize a b = case runParser sanitizer None a b of
  Left err -> error $ show err
  Right s  -> s


sanitizer :: Parsec BS.ByteString UState BS.ByteString
sanitizer = do
  xs <- many entry
  eof
  return $ pack $ concat $ map (intercalate "\n " . chunksOf 75) xs

entry :: Parsec BS.ByteString UState String
entry = do
  n <- name
  _ <- char ':'
  v <- case n of
    "DESCRIPTION" -> value ";"
    _             -> value ""
  return $ n ++ ":" ++ v ++ "\n"


name :: Parsec BS.ByteString UState String
name = do
  n   <- many1 (noneOf ":\r\n")
  ns  <- many cont
  return $ n ++ concat ns
  where
    cont = do
      _ <- endOfLine
      _ <- char ' '
      a <- many (noneOf ":\r\n")
      return a


value :: String -> Parsec BS.ByteString UState String
value escape = do
  a <- many schar
  _ <- endOfLine
  b <- many cont
  return $ concat a ++ concat b
  where
    cont = do
      _ <- char ' '
      a <- many schar
      _ <- endOfLine
      return $ concat a
    schar = do
      a <- anyNonEOL
      getState >>= \state -> if state == Escaped
        then do setState None
                return ['\\', a]
        else case a of
               '\\'                -> putState Escaped >> return ""
               x | x `elem` escape -> return ['\\', x]
               x                   -> return [x]


anyNonEOL :: Parsec BS.ByteString UState Char
anyNonEOL = noneOf "\r\n"


data UState = None | Escaped deriving (Eq, Ord, Show)
