module Test.Main where

import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Formatter.DateTime as DateTimeFormatter
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (Unit, discard, map, (<<<))
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import UnixTime (fromUnixTime, fromUnixTime')

format :: DateTime -> String
format =
  DateTimeFormatter.format
    ( List.fromFoldable
        [ DateTimeFormatter.YearFull
        , DateTimeFormatter.Placeholder "-"
        , DateTimeFormatter.MonthTwoDigits
        , DateTimeFormatter.Placeholder "-"
        , DateTimeFormatter.DayOfMonthTwoDigits
        , DateTimeFormatter.Placeholder "T"
        , DateTimeFormatter.Hours24
        , DateTimeFormatter.Placeholder ":"
        , DateTimeFormatter.MinutesTwoDigits
        , DateTimeFormatter.Placeholder ":"
        , DateTimeFormatter.SecondsTwoDigits
        , DateTimeFormatter.Placeholder "Z"
        ]
    )

main :: Effect Unit
main = runTest do
  test "fromUnixTime" do
    Assert.equal
      (Just "2019-02-02T00:29:11Z")
      (map (format <<< toDateTime) (fromUnixTime 1549067351))

  test "fromUnixTime'" do
    Assert.equal
      (Just "2019-02-02T00:29:11Z")
      (map format (fromUnixTime' 1549067351))
