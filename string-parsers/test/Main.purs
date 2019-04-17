module Test.Main where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Effect (Effect)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert
import Test.Unit.Main as TestUnitMain

data YMD = YMD Int Int Int

derive instance eqYMD :: Eq YMD

instance showYMD :: Show YMD where
  show (YMD y m d) = "(YMD " <> show y <> " " <> show m <> " " <> show d <> ")"

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

main :: Effect Unit
main = TestUnitMain.runTest do
  TestUnit.test "parseYMD" do
    Assert.equal
      (Maybe.Just (YMD 2000 1 2))
      (parseYMD "2000/01/02")
