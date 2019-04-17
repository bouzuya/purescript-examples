-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr
-- https://ja.stackoverflow.com/q/54232/2341
module Main
  ( main
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert
import Test.Unit.Main as TestUnitMain

substr :: Int -> Int -> String -> String
substr start length s
  | length <= 0 = ""
  | otherwise =
    let start' = max 0 (start + (if start < 0 then CodeUnits.length s else 0))
    in Maybe.fromMaybe "" (CodeUnits.slice start' (start' + length) s)

substr' :: Int -> String -> String
substr' start s =
  let start' = max 0 (start + (if start < 0 then CodeUnits.length s else 0))
  in CodeUnits.drop start' s

main :: Effect Unit
main = TestUnitMain.runTest do
  TestUnit.test "substr" do
    let str = "Mozilla"
    Assert.equal "oz" (substr 1 2 str)
    Assert.equal "zilla" (substr' 2 str)
    let aString = "Mozilla"
    Assert.equal "M" (substr 0 1 aString)
    Assert.equal "" (substr 1 0 aString)
    Assert.equal "a" (substr (-1) 1 aString)
    Assert.equal "" (substr 1 (-1) aString)
    Assert.equal "lla" (substr' (-3) aString)
    Assert.equal "ozilla" (substr' 1 aString)
    Assert.equal "Mo" (substr (-20) 2 aString)
    Assert.equal "" (substr 20 2 aString)
