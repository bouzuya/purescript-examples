module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Halogen (ComponentHTML)
import Halogen.HTML (body, div_, head, html, meta, text, title)
import Halogen.HTML.Properties (charset) as P
import Halogen.HTML.Renderer.String (renderHTML) as R
import Prelude (Unit, ($))

-- <html>...<body><div>HELLO</div></body></html>
-- type ComponentHTML f = HTML Void (f Unit)
root :: forall f. ComponentHTML f
root =
  html
    []
    [ head []
           [ meta [ P.charset "utf-8" ]
           , title [] [ text "TITLE" ]
           ]
    , body []
           [ div_ [ text "HELLO" ]
           ]
    ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ R.renderHTML root
