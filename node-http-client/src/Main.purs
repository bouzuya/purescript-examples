module Main where

import Prelude

import Data.Options (Options, assoc)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestOptions, hostname, method, path, port, protocol, request, requestAsStream, responseAsStream, responseHeaders, statusCode)
import Node.Stream (end, onDataString)

options :: Options RequestOptions
options =
  assoc protocol "http:" <>
  assoc hostname "blog.bouzuya.net" <>
  assoc port 80 <>
  assoc method "GET" <>
  assoc path "/posts.json"

main :: Effect Unit
main = do
  req <- request options \res -> do
    logShow $ statusCode res
    logShow $ responseHeaders res
    onDataString (responseAsStream res) UTF8 logShow
  end (requestAsStream req) (pure unit)
  logShow "Hello sailor!"
