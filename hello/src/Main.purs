module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit)

main :: Eff (console :: CONSOLE) Unit
main = log "Hello World!"
