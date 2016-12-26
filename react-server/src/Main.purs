module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, ($), (<>))
import React (ReactClass, ReactElement, createClassStateless, createElement, createElementTagName)
import React.DOM (body, div, h1, head, html, meta, text, title)
import React.DOM.Props (charSet, className, lang)
import ReactDOM (renderToStaticMarkup)

-- <h1>Hello, {name}</h1>
welcomeClass :: ReactClass { name :: String }
welcomeClass = createClassStateless f
  where
    f :: { name :: String } -> ReactElement
    f { name } = h1 [] [ text $ "Hello, " <> name ]

-- <div class="message">Hello, React!</div>
message :: ReactElement
message = div
  [ className "message" ]
  [ text "Hello, React!" ]

-- <div>{welcome}{message}</div>
app :: ReactElement
app = createElementTagName
  "div"
  {}
  [ message
  , createElement welcomeClass { name: "Sara" } []
  ]

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
