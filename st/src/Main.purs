module Main where

import Prelude (Unit, ($), (*), bind, show)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.ST (ST, modifySTRef, newSTRef, pureST, readSTRef, runST, writeSTRef)

example1 :: forall e h. Eff (console :: CONSOLE, st :: ST h | e) Unit
example1 = do
  ref <- newSTRef 123
  n1 <- readSTRef ref
  log $ show n1
  n2 <- writeSTRef ref 456
  log $ show n2
  n3 <- modifySTRef ref \x -> x * 2
  log $ show n3

example2 :: Int
example2 = pureST do
  ref <- newSTRef 1
  modifySTRef ref \x -> x * 2
  modifySTRef ref \x -> x * 2
  modifySTRef ref \x -> x * 2
  readSTRef ref

example3 :: Int
example3 = runPure $ runST do
  ref <- newSTRef 2
  modifySTRef ref \x -> x * 2
  modifySTRef ref \x -> x * 2
  modifySTRef ref \x -> x * 2
  readSTRef ref

main :: forall e h. Eff (console :: CONSOLE, st :: ST h | e) Unit
main = do
  example1
  log $ show example2
  log $ show example3
