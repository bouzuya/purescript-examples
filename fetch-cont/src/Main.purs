module Main where

import Control.Monad.Cont (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Fetch (HTTP, fetch)
import Prelude (Unit)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = runContT (fetch "http://blog.bouzuya.net/posts.json") \e ->
  case e of
    Left _ -> log "error"
    Right text -> log text
