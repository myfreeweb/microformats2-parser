{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE Safe, NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TypeFamilies #-}

module Data.Microformats2.Parser.Date where

import           Prelude.Compat
import           Control.Applicative
import           Control.Error.Util (hush)
import           Text.Printf
import           Data.Maybe
import           Data.Foldable
import           Data.Attoparsec.Text
import qualified Data.Time.Calendar as C
import qualified Data.Time.Calendar.OrdinalDate as O
import qualified Data.Text as T

data Date = Date Int Int Int
instance Show Date where
  show (Date y m d) = printf "%d-%02d-%02d" y m d

data HourType = TwentyFourHour | AMHour | PMHour
data Time = Time Int Int Int
instance Show Time where
  show (Time h m s) = printf "%02d:%02d:%02d" h m s

data DateTime = DateTime Date Time
instance Show DateTime where
  show (DateTime d t) = show d ++ "T" ++ show t

data ZoneType = Plus | Minus
data Zone = Zone ZoneType Int Int
instance Show Zone where
  show (Zone Plus  h m) = printf "+%02d:%02d" h m
  show (Zone Minus h m) = printf "-%02d:%02d" h m

data TimeZone = TimeZone Time Zone
instance Show TimeZone where
  show (TimeZone t z) = show t ++ show z

data DateTimeZone = DateTimeZone DateTime Zone
instance Show DateTimeZone where
  show (DateTimeZone dt z) = show dt ++ show z

data DTPart = DatePart Date | TimePart Time | ZonePart Zone | TimeZonePart TimeZone | DateTimePart DateTime | DateTimeZonePart DateTimeZone
instance Show DTPart where
  show (DatePart d) = show d
  show (TimePart t) = show t
  show (ZonePart z) = show z
  show (TimeZonePart tz) = show tz
  show (DateTimePart dt) = show dt
  show (DateTimeZonePart dtz) = show dtz

isDatePart, isTimePart, isZonePart, isTimeZonePart, isDateTimePart, isDateTimeZonePart ∷ DTPart → Bool
isDatePart (DatePart _) = True
isDatePart _ = False
isTimePart (TimePart _) = True
isTimePart _ = False
isZonePart (ZonePart _) = True
isZonePart _ = False
isTimeZonePart (TimeZonePart _) = True
isTimeZonePart _ = False
isDateTimePart (DateTimePart _) = True
isDateTimePart _ = False
isDateTimeZonePart (DateTimeZonePart _) = True
isDateTimeZonePart _ = False

parseDate ∷ Parser Date
parseDate = parseDate'
  where parseDate' = do
          year ← read <$> count 4 digit
          char '-'
          parseMMDD year <|> parseDDD year
        parseMMDD year = do
          mm ← read <$> count 2 digit
          char '-'
          dd ← read <$> count 2 digit
          return $ Date year mm dd
        parseDDD year = do
          ddd ← read <$> count 3 digit
          let (_, mm, dd) = C.toGregorian $ O.fromOrdinalDate (fromIntegral year) ddd
          return $ Date year mm dd

parseHourType ∷ Parser HourType
parseHourType =
      ((char 'a' <|> char 'A') >> option '.' (char '.') >> (char 'm' <|> char 'M') >> option '.' (char '.') >> return AMHour)
  <|> ((char 'p' <|> char 'P') >> option '.' (char '.') >> (char 'm' <|> char 'M') >> option '.' (char '.') >> return PMHour)

parseTime ∷ Parser Time
parseTime = do
  hrs  ← read <$> count 2 digit
  mins ← option 0 $ char ':' >> read <$> count 2 digit
  secs ← option 0 $ char ':' >> read <$> count 2 digit
  htyp ← option TwentyFourHour parseHourType
  let hrs' = case (hrs, htyp) of
               (12, AMHour) → 00
               (x,  PMHour) | x < 12 → x + 12
               (x,  _) → x
  return $ Time hrs' mins secs

parseZone ∷ Parser Zone
parseZone = (char 'Z' >> return (Zone Plus 0 0)) <|> parseZone'
  where parseZone' = do
          htyp ← (char '+' >> return Plus) <|> (char '-' >> return Minus)
          hrs  ← read <$> count 2 digit
          mins ← option 0 $ option ':' (char ':') >> read <$> count 2 digit
          return $ Zone htyp hrs mins

parseTimeZone ∷ Parser TimeZone
parseTimeZone = do
  t ← parseTime
  z ← parseZone
  return $ TimeZone t z

parseDateTime ∷ Parser DateTime
parseDateTime = do
  d ← parseDate
  option 'T' $ char 'T' <|> char ' '
  t ← parseTime
  return $ DateTime d t

parseDateTimeZone ∷ Parser DateTimeZone
parseDateTimeZone = do
  dt ← parseDateTime
  z ← parseZone
  return $ DateTimeZone dt z

parseDTPart ∷ Parser DTPart
parseDTPart =
      (DateTimeZonePart <$> parseDateTimeZone)
  <|> (DateTimePart <$> parseDateTime)
  <|> (DatePart <$> parseDate)
  <|> (TimeZonePart <$> parseTimeZone)
  <|> (TimePart <$> parseTime)
  <|> (ZonePart <$> parseZone)

parseDTParts ∷ (Traversable φ, Monoid (φ DTPart)) ⇒ φ T.Text → φ DTPart
parseDTParts = fromMaybe mempty . sequence . fmap (hush . parseOnly parseDTPart)

normalizeDTParts ∷ (Foldable φ) ⇒ φ DTPart → Maybe DTPart
normalizeDTParts ps = asum [ find isDateTimeZonePart ps, findDateTime, findDateAndTime, find isDatePart ps, find isTimeZonePart ps, find isTimePart ps ]
  where findDateTime = do
          (DateTimePart dt) ← find isDateTimePart ps
          return $ case find isZonePart ps of
            Just (ZonePart z) → DateTimeZonePart $ DateTimeZone dt z
            _ → DateTimePart dt
        findDateAndTime = do
          (DatePart d) ← find isDatePart ps
          case find isTimeZonePart ps of
            Just (TimeZonePart (TimeZone t z)) → return $ DateTimeZonePart $ DateTimeZone (DateTime d t) z
            _ → findTime d
        findTime d = do
          (TimePart t) ← find isTimePart ps
          return $ case find isZonePart ps of
            Just (ZonePart z) → DateTimeZonePart $ DateTimeZone (DateTime d t) z
            _ → DateTimePart $ DateTime d t
