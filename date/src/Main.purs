module Main where

import Prelude

import Control.Alternative (pure)
import Data.Date (Date, Month, Year, diff, exactDate) as D
import Data.DateTime (DateTime(..)) as DT
import Data.DateTime.Instant (fromDateTime, instant, unInstant) as DTI
import Data.Enum (toEnum)
import Data.Maybe (Maybe)
import Data.Show (show)
import Data.Time (Hour, Millisecond, Minute, Time(..), diff) as T
import Data.Time.Duration (Days, Hours, Milliseconds, Minutes, Seconds)
import Effect (Effect)
import Effect.Class.Console (logShow)

-- (Just (Date (Year 2016) February (Day 13)))
example1 :: Effect Unit
example1 =
  let
    md :: Maybe D.Date
    md = do
      year <- toEnum 2016
      month <- toEnum 2
      dayOfMonth <- toEnum 13
      D.exactDate year month dayOfMonth
  in
    do
      logShow $ show md

-- (Just (Days -1.0))
-- (Just (Seconds -86400.0))
example2 :: Effect Unit
example2 =
  let
    my :: Maybe D.Year
    my = toEnum 2006
    mm :: Maybe D.Month
    mm = toEnum 1
    md1 :: Maybe D.Date
    md1 = do
      y <- my
      m <- mm
      d <- toEnum 2
      D.exactDate y m d
    md2 :: Maybe D.Date
    md2 = do
      y <- my
      m <- mm
      d <- toEnum 3
      D.exactDate y m d
    duration1 :: Maybe Days
    duration1 = do
      d1 <- md1
      d2 <- md2
      pure $ D.diff d1 d2
    duration2 :: Maybe Seconds
    duration2 = D.diff <$> md1 <*> md2
  in
    do
      logShow $ show duration1
      logShow $ show duration2

-- (Just (Time (Hour 15) (Minute 4) (Second 5) (Millisecond 0)))
example3 :: Effect Unit
example3 =
  let
    mt1 :: Maybe T.Time
    mt1 = do
      h <- toEnum 15
      m <- toEnum 4
      s <- toEnum 5
      ms <- toEnum 0
      pure $ T.Time h m s ms -- exactTime は存在しない
  in
    logShow $ show mt1

-- (Just (Hours -0.0002777777777777778))
-- (Just (Minutes -0.016666666666666666))
-- (Just (Seconds -1.0))
-- (Just (Milliseconds -1000.0))
example4 :: Effect Unit
example4 =
  let
    mh :: Maybe T.Hour
    mh = toEnum 15
    mm :: Maybe T.Minute
    mm = toEnum 4
    mms :: Maybe T.Millisecond
    mms = toEnum 8
    mt1 :: Maybe T.Time
    mt1 = do
      h <- mh
      m <- mm
      s <- toEnum 5
      ms <- mms
      pure $ T.Time h m s ms -- exactTime は存在しない
    mt2 :: Maybe T.Time
    mt2 = do
      h <- mh
      m <- mm
      s <- toEnum 6
      ms <- mms
      pure $ T.Time h m s ms -- exactTime は存在しない
    duration1 :: Maybe Hours
    duration1 = do
      t1 <- mt1
      t2 <- mt2
      pure $ T.diff t1 t2
    duration2 :: Maybe Minutes
    duration2 = T.diff <$> mt1 <*> mt2
    duration3 :: Maybe Seconds
    duration3 = T.diff <$> mt1 <*> mt2
    duration4 :: Maybe Milliseconds
    duration4 = T.diff <$> mt1 <*> mt2
  in
    do
      logShow $ show duration1
      logShow $ show duration2
      logShow $ show duration3
      logShow $ show duration4

-- (Just (DateTime (Date (Year 2006) January (Day 2)) (Time (Hour 15) (Minute 4) (Second 5) (Millisecond 0))))
-- (Just (Instant (Milliseconds 1136214245000.0)))
-- (Just (Instant (Milliseconds 1136214245000.0)))
example5 :: Effect Unit
example5 =
  let
    mdt = do -- Maybe DateTime
      dy <- toEnum 2006
      dm <- toEnum 1
      dd <- toEnum 2
      d <- D.exactDate dy dm dd
      th <- toEnum 15
      tm <- toEnum 4
      ts <- toEnum 5
      tms <- toEnum 0
      t <- pure $ T.Time th tm ts tms
      pure $ DT.DateTime d t
    mi = do -- Maybe Instant
      dt <- mdt
      pure $ DTI.fromDateTime dt
    mi' = do -- Maybe Instant
      i <- mi --
      ms <- pure $ DTI.unInstant i -- uninstant :: Instant -> Milliseconds
      i' <- DTI.instant ms -- instant :: Milliseconds -> Maybe Instant
      pure i'
  in
    do
      logShow $ show mdt
      logShow $ show mi
      logShow $ show mi'

main :: Effect Unit
main = do
  example1
  example2
  example3
  example4
  example5
