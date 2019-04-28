module Main where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, getField, jsonParser)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import ForeignGeneric (read)
import Partial.Unsafe (unsafePartial)

data Entry = Entry { title :: String }
instance entryShow :: Show Entry where
  show (Entry e) = e.title

instance decodeEntry :: DecodeJson Entry where
  decodeJson js = do
    obj <- decodeJson js
    title <- getField obj "title"
    pure $ Entry { title }

json :: String
json = "[{\"title\":\"title1\",\"date\":\"2017-01-01\"}]"

main :: Effect Unit
main = do
  logShow $ do 
    unsafePartial case decodeJson =<< jsonParser """[{"title":"title1"}]""" of
      Left _ -> "parse error"
      Right ([e::Entry]) -> show e
  -- use Foreign-generic
  read
