module Test.Main where

import Prelude

import Data.Foldable as Foldable
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Console (log)
import JSONStableStringify (jsonStableStringify)
import Main as Main
import Simple.JSON (class WriteForeign)
import Simple.JSON as SimpleJSON
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  Main.main
  log "You should add some tests."
  runTest do
    suite "jsonStableStringify" do
      suite "spaces: 0" do
        let
          f :: forall a. WriteForeign a => a -> String
          f = jsonStableStringify 0 <<< SimpleJSON.write
        test "array" do
          Assert.equal "[1,2,3]" (f [1, 2, 3])
          Assert.equal "[[1,2],[3,4],[5,6]]" (f [[1, 2], [3, 4], [5, 6]])
        test "boolean" do
          Assert.equal "false" (f false)
          Assert.equal "true" (f true)
        test "null" do
          Assert.equal "null" (f (Nullable.null :: Nullable Int))
        test "number" do
          Assert.equal "1" (f 1)
          Assert.equal "1" (f 1.0)
          Assert.equal "1.1" (f 1.1)
        test "object" do
          Assert.equal "{\"a\":1,\"b\":{\"c\":2}}" (f { b: { c: 2 }, a: 1 })
        test "string" do
          Assert.equal "\"b\"" (f "b")
      suite "spaces: 2" do
        let
          f :: forall a. WriteForeign a => a -> String
          f = jsonStableStringify 2 <<< SimpleJSON.write
        test "array" do
          Assert.equal "[\n  1,\n  2,\n  3\n]" (f [1, 2, 3])
          Assert.equal
            (Foldable.intercalate
              "\n"
              [ "["
              , "  ["
              , "    1,"
              , "    2"
              , "  ],"
              , "  ["
              , "    3,"
              , "    4"
              , "  ],"
              , "  ["
              , "    5,"
              , "    6"
              , "  ]"
              , "]"
              ])
            (f [[1, 2], [3, 4], [5, 6]])
        test "boolean" do
          Assert.equal "false" (f false)
          Assert.equal "true" (f true)
        test "null" do
          Assert.equal "null" (f (Nullable.null :: Nullable Int))
        test "number" do
          Assert.equal "1" (f 1)
          Assert.equal "1" (f 1.0)
          Assert.equal "1.1" (f 1.1)
        test "object" do
          Assert.equal
            "{\n  \"a\": 1,\n  \"b\": {\n    \"c\": 2\n  }\n}"
            (f { b: { c: 2 }, a: 1 })
        test "string" do
          Assert.equal "\"b\"" (f "b")

