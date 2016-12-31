module Fetch (HTTP, fetch) where

import Prelude (Unit, (<<<))

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))

foreign import data HTTP :: !

foreign import fetchImpl ::
  forall eff.
    String
    -> (String -> Eff (http :: HTTP | eff) Unit)
    -> (String -> Eff (http :: HTTP | eff) Unit)
    -> (Eff (http :: HTTP | eff) Unit)

fetch :: forall eff. String -> ContT Unit (Eff (http :: HTTP | eff)) (Either String String)
fetch req = ContT \k -> fetchImpl req (k <<< Right) (k <<< Left)
