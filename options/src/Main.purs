module Main where

import Control.Monad.Cont (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Options ((:=))
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Fetch (fetch)
import Fetch.Options (FetchOptions, Method(..), headers, method, url)
import Node.HTTP (HTTP)
import Prelude (Unit, (<>))

options :: FetchOptions
options =
  headers := fromFoldable [ Tuple "Accept" "application/json"
                          , Tuple "User-Agent" "bbn-server"
                          ]
  <> method := GET
  <> url := "http://blog.bouzuya.net/posts.json"

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = runContT (fetch options) \e ->
  case e of
    Left _ -> log "error"
    Right text -> log text
