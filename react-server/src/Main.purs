module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import React (ReactElement, createElementTagName)
import ReactDOM (renderToStaticMarkup)

-- <div></div>
root :: ReactElement
root = createElementTagName "div" {} []

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ renderToStaticMarkup root
