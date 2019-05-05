module Error where

import Prelude

import Data.Either (Either(..), fromLeft, isLeft)
import Effect.Aff (Aff, attempt, error, makeAff, nonCanceler)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)

aff :: Aff Unit
aff = do
  logShow "Error.aff -----"
  message1 <- makeAff \success -> do 
    success $ Right "123"
    pure nonCanceler
  logShow message1
  e <- attempt $ makeAff \cb -> do
    cb <<< Left $ error $ "ERROR!!"
    pure nonCanceler

  logShow $ show $ isLeft e -- true
  logShow $ show $ unsafePartial $ fromLeft e -- "ERROR!"
  logShow "OK"

-- Error.aff -----
-- 123
-- true
-- Error: ERROR!!
--     at Object.exports.error ...
--     ...
-- OK
