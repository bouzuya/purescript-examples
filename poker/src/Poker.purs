module Poker
  ( combination
  , hands
  , main
  ) where

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Effect (Effect, foreachE)
import Effect.Class.Console (log)
import Prelude (Unit, bind, map, mempty, pure, (-), (<>))

combination :: forall a. Int -> Array a -> Array (Array a)
combination = f
  where
    f _ [] = mempty
    f 0 _ = Array.singleton mempty
    f 1 a = map Array.singleton a
    f n a = fromMaybe mempty do
      x <- Array.head a
      xs <- Array.tail a
      pure ((map (Array.cons x) (f (n - 1) xs)) <> (f n xs))

hands :: Array String
hands =
  let
    cards = do
      suits <- ["S", "D", "H", "C"]
      numbers <- ["A", "2", "3", "4", "5", "6", "7", "8", "9", "X", "J", "Q", "K"]
      pure (suits <> numbers)
  in
    map (Array.intercalate "") (combination 5 cards)

main :: Effect Unit
main = foreachE hands log
