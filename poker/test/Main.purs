module Test.Main where

import Effect (Effect)
import Prelude (Unit)
import Test.Poker as Poker
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Poker.tests
