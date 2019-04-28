module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (ListenOptions, Request, Response, createServer, listen, requestURL, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)

listener :: Request -> Response -> Effect Unit
listener req res = do
  let path = requestURL req
  setStatusCode res 200
  setHeader res "Content-Type" "text/plain"
  let w = responseAsStream res
  _ <- writeString w UTF8 path (pure unit)
  end w (pure unit)

main :: Effect Unit
main = do
    server <- createServer listener
    listen server options done
  where
    port :: Int
    port = 8080
    done :: Effect Unit
    done = logShow ("listening on port " <> show port)
    options :: ListenOptions
    options = { hostname: "localhost", port: port, backlog: Nothing }
