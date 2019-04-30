module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow "You should add some tests."
