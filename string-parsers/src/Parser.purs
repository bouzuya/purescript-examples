module Parser
  ( parseDateTime
  , parseDateTime'
  , parseHMS
  , parseYMD
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String.CodeUnits as CodeUnits
import Data.Traversable as Traversable
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import NoParser as NoParser
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, char)
import Text.Parsing.StringParser.Combinators ((<?>))

count :: forall a. Int -> Parser a -> Parser (Array a)
count n p = Traversable.sequence (Array.replicate n p)

digits :: Int -> Parser Char -> Parser Int
digits n p = do
  s <- map CodeUnits.fromCharArray (count n p)
  case Int.fromString s of
    Maybe.Nothing -> fail "expected int"
    Maybe.Just i -> pure i

parseHMS :: String -> Maybe NoParser.HMS
parseHMS s = Either.hush (runParser parserHMS s)

parserHMS :: Parser NoParser.HMS
parserHMS =
  NoParser.HMS
    <$> (digits 2 (anyDigit <?> "hour"))
    <* (char ':' <?> "delim hh:mm")
    <*> (digits 2 (anyDigit <?> "minute"))
    <* (char ':' <?> "delim mm:ss")
    <*> (digits 2 (anyDigit <?> "second"))

parseYMD :: String -> Maybe NoParser.YMD
parseYMD s = Either.hush (runParser parserYMD s)

parserYMD :: Parser NoParser.YMD
parserYMD =
  NoParser.YMD
    <$> (digits 4 (anyDigit <?> "year"))
    <* (char '/' <?> "delim yyyy/mm")
    <*> (digits 2 (anyDigit <?> "month"))
    <* (char '/' <?> "delim mm/dd")
    <*> (digits 2 (anyDigit <?> "day"))

parseDateTime :: String -> Maybe (Tuple NoParser.YMD NoParser.HMS)
parseDateTime s = Either.hush (parseDateTime' s)

parseDateTime' :: String -> Either ParseError (Tuple NoParser.YMD NoParser.HMS)
parseDateTime' = runParser parserDateTime

parserDateTime :: Parser (Tuple NoParser.YMD NoParser.HMS)
parserDateTime =
  Tuple.Tuple
    <$> (parserYMD <?> "YMD")
    <* (char ' ' <?> "Delim YMD HMS")
    <*> (parserHMS <?> "HMS")
