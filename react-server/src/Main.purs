module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Exception (throw)
import Foreign (MultipleErrors, readString, unsafeToForeign)
import Foreign.Index (readProp)
import React (ReactClass, ReactElement, ReactThis, component, createElementTagName, createLeafElement, getProps, getState, statelessComponent, writeState)
import React.DOM as RD
import React.DOM.Props as RP
import React.SyntheticEvent (NativeEvent, SyntheticEvent_)
import ReactDOM (render, renderToStaticMarkup)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type InputProps = { label :: String, value :: String }
type InputState = { value :: String }

-- <label>
--   <span class="label">{label}</span>
--   <span class="value"><input name="name" value="{value}" onchange="..." /></span>
--   <span class="result">{value}</span>
-- </label>
inputClass :: ReactClass InputProps
inputClass = component "inputSpec" \this -> do
  -- init <- getInitialState this
  pure { state: {value: ""}--init
       , render: render this
       , componentDidMount: getInitialState this
       }
  where
  getInitialState :: ReactThis InputProps InputState -> Effect Unit
  getInitialState = \this -> do
    props <- getProps this
    writeState this { value: props.value }
    pure unit
  
  render :: ReactThis InputProps InputState -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $ RD.label []
           [ RD.span [ RP.className "label" ]
                     [ RD.text props.label ]
           , RD.span [ RP.className "value" ]
                     [ RD.input [ RP.name props.label
                                , RP.onChange \e -> inputChanged this e
                                , RP.value state.value
                                ] ]
           , RD.span [ RP.className "result" ]
                     [ RD.text state.value ] ]
    where
    inputChanged :: forall r. ReactThis InputProps InputState -> SyntheticEvent_ (nativeEvent :: NativeEvent | r)  -> Effect Unit
    inputChanged this e = do
      void $ case getTargetValue e of
        Left _ -> writeState this { value: ""}
        Right v -> writeState this { value: v }
      pure unit

    getTargetValue :: forall r. SyntheticEvent_ (nativeEvent :: NativeEvent | r) -> Either MultipleErrors String
    getTargetValue e =
      -- Right (unsafeCoerce e).target.value
      runExcept $ readString =<< readProp "value" =<< readProp "target" (unsafeToForeign e)

-- <h1>Hello, {name}</h1>
welcomeClass :: ReactClass { name :: String }
welcomeClass = statelessComponent f
  where
    f :: { name :: String } -> ReactElement
    f { name } = RD.h1 [] [ RD.text $ "Hello, " <> name ]

-- <div class="message">Hello, React!</div>
message :: ReactElement
message = RD.div
  [ RP.className "message" ]
  [ RD.text "Hello, React!" ]

-- <div>{message}{welcome}{input}</div>
app :: ReactElement
app = createElementTagName
  "div"
  {}
  [ message
  , createLeafElement welcomeClass { name: "Sara" }
  , createLeafElement inputClass { label: "name1", value: "John" }
  ]

-- <html lang="ja">...<body>{app}</body></html>
root :: ReactElement
root =
  RD.html
    [ RP.lang "ja" ]
    [ RD.head [] 
           [ RD.meta [ RP.charSet "utf-8" ]
           , RD.title [] [ RD.text "TITLE" ]
           ]
    , RD.body []
           [ app ]
    ]

main ::Effect Unit
main = do
  logShow $ renderToStaticMarkup root

  tr <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  void $ case tr of
    Nothing -> throw "Root element not found."
    Just r  -> render root r
  pure unit