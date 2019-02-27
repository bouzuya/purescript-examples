module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Main as Main
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "comma" do
    test "100" do
      Assert.equal
        "100"
        (Main.comma 100)

    test "1000" do
      Assert.equal
        "1,000"
        (Main.comma 1000)

    test "1000000" do
      Assert.equal
        "1,000,000"
        (Main.comma 1000000)
