module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, ($))
import React (ReactElement, createElementTagName)
import React.DOM (body, div, head, html, meta, text, title)
import React.DOM.Props (charSet, className, lang)
import ReactDOM (renderToStaticMarkup)

-- <div class="message">Hello, React!</div>
message :: ReactElement
message = div
  [ className "message" ]
  [ text "Hello, React!" ]

-- <div>{message}</div>
app :: ReactElement
app = createElementTagName "div" {} [ message ]

-- <html lang="ja">...<body>{app}</body></html>
root :: ReactElement
root =
  html
    [ lang "ja" ]
    [ head []
           [ meta [ charSet "utf-8" ] []
           , title [] [ text "TITLE" ]
           ]
    , body []
           [ app ]
    ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ renderToStaticMarkup root
