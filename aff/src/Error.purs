module Error where

import Prelude (Unit, ($), bind, show)
import Control.Monad.Aff (Aff, attempt, makeAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error) as Exception
import Data.Either (fromLeft, isLeft)
import Partial.Unsafe (unsafePartial)

aff :: forall e. Aff (console :: CONSOLE | e) Unit
aff = do
  log "Error.aff -----"
  message1 <- makeAff (\_ success -> success "123")
  log message1
  e <- attempt $ makeAff (\error _ -> error $ Exception.error "ERROR!!")
  log $ show $ isLeft e -- true
  log $ show $ unsafePartial $ fromLeft e -- "ERROR!"
  log "OK"

-- Error.aff -----
-- 123
-- true
-- Error: ERROR!!
--     at Object.exports.error ...
--     ...
-- OK
