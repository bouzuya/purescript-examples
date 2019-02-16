module JSONStableStringify (jsonStableStringify) where

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Object (Object)
import Foreign.Object as Object
import Indent (Indent)
import Indent as Indent
import Prelude (map, otherwise, (<<<), (<>), (==))

foreign import jsonStringify :: Foreign -> String

bracket :: Tuple String String -> Indent -> Array String -> String
bracket (Tuple open close) indent items =
  let
    lf = if Indent.isEnabled indent then "\n" else ""
    curr = Indent.toString indent
    next = Indent.toString (Indent.nextLevel indent)
  in
    Foldable.intercalate
      lf
      [ open
      , next <> Foldable.intercalate ("," <> lf <> next) items
      , curr <> close
      ]

jsonStableStringify :: Int -> Foreign -> String
jsonStableStringify = stringify <<< Indent.initLevel
  where
    stringify indent f
      | Foreign.isNull f = "null"
      | Foreign.isArray f =
          bracket
            (Tuple "[" "]")
            indent
            (map
              (stringify (Indent.nextLevel indent))
              (Foreign.unsafeFromForeign f :: Array Foreign))
      | Foreign.typeOf f == "object" =
          bracket
            (Tuple "{" "}")
            indent
            (map
              (\(Tuple k v) ->
                "\"" <> k <> "\"" <>
                ":" <> (if Indent.isEnabled indent then " " else "") <>
                (stringify (Indent.nextLevel indent) v))
              (Array.sortWith
                (\(Tuple k _) -> k)
                (Object.toUnfoldable
                  (Foreign.unsafeFromForeign f :: Object Foreign))))
      | otherwise = jsonStringify f
