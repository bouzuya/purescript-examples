module Parser
  ( parseDateTime
  , parseHMS
  , parseYMD
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String.CodeUnits as CodeUnits
import Data.Traversable as Traversable
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import NoParser as NoParser
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, char)

count :: forall a. Int -> Parser a -> Parser (Array a)
count n p = Traversable.sequence (Array.replicate n p)

digits :: Int -> Parser Int
digits n = do
  s <- map CodeUnits.fromCharArray (count n anyDigit)
  case Int.fromString s of
    Maybe.Nothing -> fail "expected int"
    Maybe.Just i -> pure i

parseHMS :: String -> Maybe NoParser.HMS
parseHMS s = Either.hush (runParser parserHMS s)

parserHMS :: Parser NoParser.HMS
parserHMS =
  NoParser.HMS
    <$> digits 2
    <* char ':'
    <*> digits 2
    <* char ':'
    <*> digits 2

parseYMD :: String -> Maybe NoParser.YMD
parseYMD s = Either.hush (runParser parserYMD s)

parserYMD :: Parser NoParser.YMD
parserYMD =
  NoParser.YMD
    <$> digits 4
    <* char '/'
    <*> digits 2
    <* char '/'
    <*> digits 2

parseDateTime :: String -> Maybe (Tuple NoParser.YMD NoParser.HMS)
parseDateTime s = Either.hush (runParser parserDateTime s)

parserDateTime :: Parser (Tuple NoParser.YMD NoParser.HMS)
parserDateTime =
  Tuple.Tuple
    <$> parserYMD
    <* char ' '
    <*> parserHMS
