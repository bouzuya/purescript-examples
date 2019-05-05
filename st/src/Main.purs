module Main where

import Prelude

import Control.Monad.ST (run)
import Control.Monad.ST.Internal (modify, new, read)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref as Ref

example1 :: Effect Unit
example1 = do
  ref <- Ref.new 123
  n1 <- Ref.read ref
  logShow $ show n1
  Ref.write 456 ref
  n2 <- Ref.read ref
  logShow $ show n2
  n3 <- Ref.modify (\x -> x * 2) ref
  logShow $ show n3

example2 :: Int 
example2 = run do
  ref <- new 1
  _ <- modify (\x -> x * 2) ref
  _ <- modify (\x -> x * 2) ref
  _ <- modify (\x -> x * 2) ref
  read ref

main :: Effect Unit
main = do
  example1
  logShow $ show example2

