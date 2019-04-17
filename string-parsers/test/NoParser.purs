module Test.NoParser
  ( tests
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.Tuple as Tuple
import NoParser as NoParser
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "NoParser" do
  TestUnit.test "parseYMD" do
    Assert.equal
      (Maybe.Just (NoParser.YMD 2000 1 2))
      (NoParser.parseYMD "2000/01/02")

  TestUnit.test "parseHMS" do
    Assert.equal
      (Maybe.Just (NoParser.HMS 15 0 0))
      (NoParser.parseHMS "15:00:00")

  TestUnit.test "parseDateTime" do
    Assert.equal
      (Maybe.Just (Tuple.Tuple (NoParser.YMD 1987 7 23) (NoParser.HMS 15 0 0)))
      (NoParser.parseDateTime "1987/07/23 15:00:00")

