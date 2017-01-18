module LiftEff where

import Prelude (Unit, ($), bind)
import Control.Monad.Aff (Aff, forkAff, later')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

aff :: forall e. Aff (console :: CONSOLE | e) Unit
aff = do
  liftEff $ log "LiftEff.aff -----"
  liftEff $ log "1"
  forkAff $ later' 500 $ liftEff $ log "later 500 (LiftEff.aff forked)"
  liftEff $ log "2"
  later' 1000 $ liftEff $ log "later 1000"

-- LiftEff.aff -----
-- 1
-- 2
-- later 500 (forked)
-- later 1000
