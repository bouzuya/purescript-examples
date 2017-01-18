module Main where

import Prelude (Unit, ($), bind, void)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Console (aff) as Console
import Error (aff) as Error
import LiftEff (aff) as LiftEff

aff :: forall e. Aff (console :: CONSOLE | e) Unit
aff = do
  LiftEff.aff
  Console.aff
  Error.aff

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
main = void $ launchAff aff
