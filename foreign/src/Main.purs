module Main where

import Prelude (Unit, ($), bind, pure, show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)
import Data.Show (class Show)

data Entry = Entry { title :: String }

instance entryIsForeign :: IsForeign Entry where
  read value = do
    title <- readProp "title" value
    pure $ Entry { title }

instance entryShow :: Show Entry where
  show (Entry e) = e.title

json :: String
json = "[{\"title\":\"title1\",\"date\":\"2017-01-01\"}]"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ runExcept $ (readJSON json :: F (Array Entry))
