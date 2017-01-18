module Console where

import Prelude (Unit, ($), bind)
import Control.Monad.Aff (Aff, forkAff, later')
import Control.Monad.Aff.Console (CONSOLE, log)

aff :: forall e. Aff (console :: CONSOLE | e) Unit
aff = do
  log "Console.aff -----"
  log "1"
  later' 500 $ log "later 500"
  log "2"
  forkAff $ later' 1000 $ log "later 1000 (Console.aff forked)"
  log "3"
  later' 500 $ log "later 500"
  log "4"

-- Console.aff -----
-- 1
-- later 500
-- 2
-- 3
-- later 500
-- 4
-- later 1000 (forked)
