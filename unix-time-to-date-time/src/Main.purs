module Main
  ( main
  ) where

import Effect (Effect)
import Effect.Class.Console (logShow)
import Prelude (Unit)
import UnixTime (fromUnixTime)

main :: Effect Unit
main = do
  logShow (fromUnixTime 1549067351)
