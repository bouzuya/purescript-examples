module Test.Parser
  ( tests
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable as Traversable
import Data.Tuple as Tuple
import NoParser (YMD(..))
import NoParser as NoParser
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodeUnits as StringParserCodeUnits
import Text.Parsing.StringParser.Combinators as StringParserCombinators

tests :: TestSuite
tests = TestUnit.suite "Parser" do
  TestUnit.test "char /" do
    let parser = StringParserCodeUnits.char '/'
    Assert.equal
      (Either.Right '/')
      (StringParser.runParser parser "/")
    Assert.equal
      true
      (Either.isLeft (StringParser.runParser parser ":"))
    Assert.equal
      (Either.Right '/')
      (StringParser.runParser parser "/:")
    Assert.equal
      true
      (Either.isLeft (StringParser.runParser parser ":/"))

  TestUnit.test "string foo" do
    let parser = StringParserCodeUnits.string "foo"
    Assert.equal
      (Either.Right "foo")
      (StringParser.runParser parser "foo")

  TestUnit.test "count" do
    let
      count :: forall a. Int -> Parser a -> Parser (Array a)
      count n p = Traversable.sequence (Array.replicate n p)
      parser = do
        xs <- count 3 (StringParserCodeUnits.string "a")
        _ <- StringParserCodeUnits.eof
        pure (String.joinWith "" xs)
    Assert.equal
      (Either.Right "aaa")
      (StringParser.runParser parser "aaa")

  TestUnit.test "YMD (YYYYMMDD)" do
    let
      count :: forall a. Int -> Parser a -> Parser (Array a)
      count n p = Traversable.sequence (Array.replicate n p)

      digits :: Int -> Parser Int
      digits n = do
        s <-
          map
            CodeUnits.fromCharArray
            (count n StringParserCodeUnits.anyDigit)
        case Int.fromString s of
          Maybe.Nothing -> StringParser.fail "expected int"
          Maybe.Just i -> pure i

      parser =
        YMD
          <$> digits 4
          <*> digits 2
          <*> digits 2

    Assert.equal
      (Either.Right (YMD 2019 1 2))
      (StringParser.runParser parser "20190102")

  TestUnit.test "string foo eof" do
    let
      parser = do
        s <- StringParserCodeUnits.string "foo"
        StringParserCodeUnits.eof
        pure s
    Assert.equal
      (Either.Right "foo")
      (StringParser.runParser parser "foo")
    Assert.equal
      true
      (Either.isLeft (StringParser.runParser parser "foobar"))
    Assert.equal
      true
      (Either.isLeft (StringParser.runParser parser "barfoo"))

  TestUnit.test "int" do
    let
      parser = do
        sign <-
          map
            (Maybe.maybe "" identity)
            (StringParserCombinators.optionMaybe
              (StringParserCodeUnits.string "-"))
        digits <-
          StringParserCombinators.many1Till
            StringParserCodeUnits.anyDigit
            StringParserCodeUnits.eof
        Maybe.maybe'
          (\_ -> StringParser.fail "Expected int")
          pure
          (Int.fromString
            (sign <> (Foldable.foldMap CodeUnits.singleton digits)))
    Assert.equal
      (Either.Right 123)
      (StringParser.runParser parser "123")
    Assert.equal
      (Either.Right (top :: Int))
      (StringParser.runParser parser (show (top :: Int)))
    Assert.equal
      (Either.Right (bottom :: Int))
      (StringParser.runParser parser (show (bottom :: Int)))

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
