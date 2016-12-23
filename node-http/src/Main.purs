module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(Nothing))
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, ListenOptions, Request, Response, createServer, listen, requestURL, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)
import Prelude (Unit, (<>), bind, pure, show, unit)

listener :: forall e. Request -> Response -> Eff (http :: HTTP | e) Unit
listener req res = do
  let path = requestURL req
  setStatusCode res 200
  setHeader res "Content-Type" "text/plain"
  let w = responseAsStream res
  writeString w UTF8 path (pure unit)
  end w (pure unit)

main :: forall eff. Eff (console :: CONSOLE, http :: HTTP | eff) Unit
main = do
    server <- createServer listener
    listen server options done
  where
    port :: Int
    port = 8080
    done :: forall e. Eff (console :: CONSOLE | e) Unit
    done = log ("listening on port " <> show port)
    options :: ListenOptions
    options = { hostname: "localhost", port: port, backlog: Nothing }
