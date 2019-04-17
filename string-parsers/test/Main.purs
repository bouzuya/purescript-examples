module Test.Main where

import Prelude

import Effect (Effect)
import Test.NoParser as NoParser
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  NoParser.tests
