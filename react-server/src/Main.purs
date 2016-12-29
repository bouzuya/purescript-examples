module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors, toForeign)
import Data.Foreign.Class (read)
import Data.Foreign.Index (prop)
import Prelude (Unit, ($), (<>), bind, pure, void)
import React (Event, ReactClass, ReactElement, ReactState, ReactThis, Read, Write, createClass, createClassStateless, createElement, createElementTagName, getProps, readState, spec', writeState)
import React.DOM (body, div, h1, head, html, input, label, meta, span, text, title)
import React.DOM.Props (charSet, className, lang, name, onChange, value)
import ReactDOM (renderToStaticMarkup)

type InputProps = { label :: String, value :: String }
type InputState = { value :: String }

getTargetValue :: Event -> Either MultipleErrors String
getTargetValue e = runExcept do
  target <- prop "target" (toForeign e)
  value <- prop "value" target
  read value

inputChanged :: forall eff props.
  ReactThis props InputState
  -> Event
  -> Eff (state :: ReactState ( read :: Read, write :: Write) | eff) Unit
inputChanged this event = do
  state <- readState this
  void $ case getTargetValue event of
    Left _ -> writeState this state
    Right v -> writeState this { value: v }

-- <label>
--   <span class="label">{label}</span>
--   <span class="value"><input name="name" value="{value}" onchange="..." /></span>
--   <span class="result">{value}</span>
-- </label>
inputClass :: ReactClass InputProps
inputClass = createClass inputSpec
  where
    getInitialState = \this -> do
      props <- getProps this
      pure { value: props.value }
    inputSpec = spec' getInitialState render
    render = \this -> do
      props <- getProps this
      state <- readState this
      pure $ label []
                   [ span [ className "label" ]
                          [ text props.label ]
                   , span [ className "value" ]
                          [ input [ name props.label
                                  , onChange (inputChanged this)
                                  , value state.value
                                  ]
                                  [] ]
                   , span [ className "result" ]
                          [ text state.value ] ]

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

-- <div>{message}{welcome}{input}</div>
app :: ReactElement
app = createElementTagName
  "div"
  {}
  [ message
  , createElement welcomeClass { name: "Sara" } []
  , createElement inputClass { label: "name1", value: "John" } []
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
