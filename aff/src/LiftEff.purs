module LiftEff where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), delay, forkAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)

aff :: Aff Unit
aff = do
  liftEffect $ logShow "LiftEff.aff -----"
  liftEffect $ logShow "1"
  _ <- forkAff do
    delay (Milliseconds 500.0)
    liftEffect $ logShow "later 500 (LiftEff.aff forked)"
  liftEffect $ logShow "2"
  delay (Milliseconds 1000.0)
  liftEffect $ logShow "later 1000"

-- LiftEff.aff -----
-- 1
-- 2
-- later 500 (forked)
-- later 1000
