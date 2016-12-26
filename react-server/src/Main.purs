module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, ($))
import React (ReactElement, createElementTagName)
import React.DOM (div, text)
import React.DOM.Props (className)
import ReactDOM (renderToStaticMarkup)

-- <div class="message">Hello, React!</div>
message :: ReactElement
message = div
  [ className "message" ]
  [ text "Hello, React!" ]

-- <div>{message}</div>
root :: ReactElement
root = createElementTagName "div" {} [message]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ renderToStaticMarkup root
