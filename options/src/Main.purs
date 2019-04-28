module Main where

import Prelude

import Control.Monad.Cont (runContT)
import Data.Either (Either(..))
import Data.Options ((:=), Options)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Fetch (fetch)
import Fetch.Options (FetchOptions, Method(..), headers, method, url)
import Foreign.Object (fromFoldable)

options :: Options FetchOptions
options =
  headers := fromFoldable [ Tuple "Accept" "application/json"
                          , Tuple "User-Agent" "bbn-server"
                          ]
  <> method := GET
  <> url := "http://blog.bouzuya.net/posts.json"

main :: Effect Unit
main = runContT (fetch options) \e ->
  case e of
    Left _ -> logShow "error"
    Right text -> logShow text
