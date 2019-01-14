module Test.Poker where

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Poker (combination, hands)
import Prelude (discard, negate)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Poker" do
  test "combination" do
    let f = combination
    Assert.equal [] (f 5 [1, 2, 3, 4])
    Assert.equal [] (f (-1) [1, 2, 3, 4])
    Assert.equal [] (f 5 ([] :: Array Int))
    Assert.equal [[]] (f 0 [1, 2, 3, 4])
    Assert.equal [[1], [2], [3], [4]] (f 1 [1, 2, 3, 4])
    Assert.equal [[1, 2, 3, 4]] (f 4 [1, 2, 3, 4])
    Assert.equal [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]] (f 3 [1, 2, 3, 4])
    Assert.equal
      [[1, 2, 3], [1, 2, 4], [1, 2, 5], [1, 3, 4], [1, 3, 5], [1, 4, 5], [2, 3, 4], [2, 3, 5], [2, 4, 5], [3, 4, 5]]
      (f 3 [1, 2, 3, 4, 5])
  test "hands" do
    let a = hands
    Assert.equal 2598960 (Array.length a)
    Assert.equal (Just "SAS2S3S4S5") (Array.head a)
