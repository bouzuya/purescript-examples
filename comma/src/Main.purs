module Main
  ( comma
  ) where

import Data.Either as Either
import Data.Formatter.Number as NumberFormatter
import Data.Int as Int
import Partial.Unsafe as Unsafe

comma :: Int -> String
comma n =
  Unsafe.unsafePartial
    (Either.fromRight
      (NumberFormatter.formatNumber
        "0,0"
        (Int.toNumber n)))
