module Main
  ( main
  ) where

import Bouzuya.DateTime (DayOfYear, WeekOfYear, Weekday)
import Bouzuya.DateTime as BouzuyaDateTime
import Data.Array as Array
import Data.Date (Day, Date, Month, Year)
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.Enum as Enum
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe as Unsafe
import Prelude (Unit, bind, bottom, discard, map, pure, show, top)

-- ある年の日を返す (bouzuya-datetime `DayOfYear` を利用)
datesInYear :: Year -> Array Date
datesInYear year =
  Array.catMaybes
    (map
      (BouzuyaDateTime.exactDateFromDayOfYear year)
      (Enum.enumFromTo bottom top :: Array DayOfYear))

-- ある年の日を返す (datetime を利用)
datesInYear' :: Year -> Array Date
datesInYear' year =
  Array.catMaybes do
    m <- Enum.enumFromTo bottom top :: Array Month
    d <- Enum.enumFromTo bottom top :: Array Day
    pure (Date.exactDate year m d)

-- ある年・月の日を返す (datetime を利用)
datesInYearMonth :: Year -> Month -> Array Date
datesInYearMonth year month =
  Array.catMaybes do
    d <- Enum.enumFromTo bottom top :: Array Day
    pure (Date.exactDate year month d)

-- ある年・週の日を返す (bouzuya-datetime `WeekOfYear` を利用)
datesInYearWeek :: Year -> WeekOfYear -> Array Date
datesInYearWeek year woy =
  Array.catMaybes do
    wd <- Enum.enumFromTo bottom top :: Array Weekday
    pure (BouzuyaDateTime.exactDateFromWeekOfYear year woy wd)

showDates :: Array Date -> String
showDates dates =
  Array.intercalate
    "\n"
    (map
      (Formatter.format
        (List.fromFoldable
          [ Formatter.YearFull
          , Formatter.Placeholder "-"
          , Formatter.MonthTwoDigits
          , Formatter.Placeholder "-"
          , Formatter.DayOfMonthTwoDigits
          ]))
        (map (\d -> DateTime d bottom) dates))

main :: Effect Unit
main = do
  let year = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 2019))
  Console.log "datesInYear"
  Console.log (showDates (datesInYear year))
  Console.log (show (Array.length (datesInYear year)))
  Console.log "datesInYear'"
  Console.log (showDates (datesInYear' year))
  Console.log (show (Array.length (datesInYear' year)))
  let month = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 2))
  Console.log "datesInYearMonth"
  Console.log (showDates (datesInYearMonth year month))
  Console.log (show (Array.length (datesInYearMonth year month)))
  let woy = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 7))
  Console.log "datesInYearWeek"
  Console.log (showDates (datesInYearWeek year woy))
  Console.log (show (Array.length (datesInYearWeek year woy)))
