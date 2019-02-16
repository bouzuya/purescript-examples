module Indent
  ( Indent
  , initLevel
  , isEnabled
  , nextLevel
  , toString
  ) where

import Data.Array as Array
import Data.Foldable as Foldable
import Prelude ((*), (+), (>))

type Level = Int
type Space = Int
data Indent = Indent Space Level

initLevel :: Space -> Indent
initLevel space = Indent space 0

isEnabled :: Indent -> Boolean
isEnabled (Indent space _) = space > 0

nextLevel :: Indent -> Indent
nextLevel (Indent space level) = Indent space (level + 1)

toString :: Indent -> String
toString (Indent space level) =
  Foldable.fold (Array.replicate (space * level) " ")
