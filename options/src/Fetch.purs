module Fetch (fetch) where

import Prelude (Unit, (<<<))

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Options (options)
import Fetch.Options (FetchOptions)
import Node.HTTP (HTTP)

foreign import fetchImpl ::
  forall eff.
    Foreign
    -> (String -> Eff (http :: HTTP | eff) Unit)
    -> (String -> Eff (http :: HTTP | eff) Unit)
    -> (Eff (http :: HTTP | eff) Unit)

fetch :: forall eff. FetchOptions -> ContT Unit (Eff (http :: HTTP | eff)) (Either String String)
fetch opts = ContT \k ->
  fetchImpl (options opts) (k <<< Right) (k <<< Left)
