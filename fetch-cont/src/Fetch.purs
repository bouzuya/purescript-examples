module Fetch (HTTP, fetch) where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Data.Either (Either(..))
import Effect (Effect)

foreign import data HTTP :: Type

foreign import fetchImpl ::
    String
    -> (String -> Effect Unit)
    -> (String -> Effect Unit)
    -> (Effect Unit)

fetch :: String -> ContT Unit Effect (Either String String)
fetch req = ContT \k -> fetchImpl req (k <<< Right) (k <<< Left)

