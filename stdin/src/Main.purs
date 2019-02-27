module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.Process as Process
import Node.Stream (Readable)
import Node.Stream as Stream

readTextFromStream :: Readable () -> Aff String
readTextFromStream r = Aff.makeAff \callback -> do
  ref <- Ref.new ""
  Stream.onDataString r Encoding.UTF8 \s -> do
    buffer <- Ref.read ref
    Ref.write (buffer <> s) ref
  Stream.onEnd r do
    buffer <- Ref.read ref
    callback (pure buffer)
  pure mempty -- canceler

main :: Effect Unit
main = Aff.launchAff_ do
  s <- readTextFromStream Process.stdin
  Console.log s
