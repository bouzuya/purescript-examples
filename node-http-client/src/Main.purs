module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Options (Options, assoc)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP)
import Node.HTTP.Client (RequestOptions, hostname, method, path, port, protocol, request, requestAsStream, responseAsStream, responseHeaders, statusCode)
import Node.Stream (end, onDataString)
import Prelude (Unit, ($), (<>), bind, pure, unit)

options :: Options RequestOptions
options =
  assoc protocol "http:" <>
  assoc hostname "blog.bouzuya.net" <>
  assoc port 80 <>
  assoc method "GET" <>
  assoc path "/posts.json"

main :: forall e. Eff (err :: EXCEPTION, console :: CONSOLE, http :: HTTP | e) Unit
main = do
  req <- request options \res -> do
    logShow $ statusCode res
    logShow $ responseHeaders res
    onDataString (responseAsStream res) UTF8 log
  end (requestAsStream req) (pure unit)
  log "Hello sailor!"
