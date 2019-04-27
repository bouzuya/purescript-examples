module Main where

import Prelude

import Control.Monad.Cont (runContT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Fetch (fetch)

main :: Effect Unit
main = runContT (fetch "http://blog.bouzuya.net/posts.json") \e ->
  case e of
    Left _ -> logShow "error"
    Right text -> logShow text
