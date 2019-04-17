module NoParser
  ( HMS(..)
  , YMD(..)
  , parseDateTime
  , parseHMS
  , parseYMD
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple

data HMS = HMS Int Int Int

derive instance eqHMS :: Eq HMS

instance showHMS :: Show HMS where
  show (HMS h m s) = "(HMS " <> show h <> " " <> show m <> " " <> show s <> ")"

data YMD = YMD Int Int Int

derive instance eqYMD :: Eq YMD

instance showYMD :: Show YMD where
  show (YMD y m d) = "(YMD " <> show y <> " " <> show m <> " " <> show d <> ")"

parseHMS :: String -> Maybe HMS
parseHMS = arrayToHMS <<< (String.split (String.Pattern ":"))
  where
    arrayToHMS :: Array String -> Maybe HMS
    arrayToHMS = case _ of
      [h, m, s] ->
        HMS
          <$> Int.fromString h
          <*> Int.fromString m
          <*> Int.fromString s
      _ -> Maybe.Nothing

parseYMD :: String -> Maybe YMD
parseYMD = arrayToYMD <<< (String.split (String.Pattern "/"))
  where
    arrayToYMD :: Array String -> Maybe YMD
    arrayToYMD = case _ of
      [y, m, d] ->
        YMD
          <$> Int.fromString y
          <*> Int.fromString m
          <*> Int.fromString d
      _ -> Maybe.Nothing

parseDateTime :: String -> Maybe (Tuple YMD HMS)
parseDateTime = arrayToDateTime <<< (String.split (String.Pattern " "))
  where
    arrayToDateTime :: Array String -> Maybe (Tuple YMD HMS)
    arrayToDateTime = case _ of
      [d, t] ->
        Tuple.Tuple
          <$> parseYMD d
          <*> parseHMS t
      _ -> Maybe.Nothing
