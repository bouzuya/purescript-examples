module Console where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), delay, forkAff)
import Effect.Class.Console (logShow)

aff :: Aff Unit
aff = do
  logShow "Console.aff -----"
  logShow "1"
  delay (Milliseconds 500.0)
  logShow "later 500"
  logShow "2"
  _ <- forkAff do
    delay (Milliseconds 1000.0)
    logShow "later 1000 (Console.aff forked)"
  logShow "3"
  delay (Milliseconds 500.0)
  logShow "later 500"
  logShow "4"

-- Console.aff -----
-- 1
-- later 500
-- 2
-- 3
-- later 500
-- 4
-- later 1000 (forked)
