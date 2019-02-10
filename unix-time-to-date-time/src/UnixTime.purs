module UnixTime
  ( fromUnixTime
  , fromUnixTime'
  ) where

import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, instant, toDateTime)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds(..), fromDuration)
import Prelude (map)

fromUnixTime :: Int -> Maybe Instant
fromUnixTime n = instant (fromDuration (Seconds (toNumber n)))

fromUnixTime' :: Int -> Maybe DateTime
fromUnixTime' n = map toDateTime (fromUnixTime n)
