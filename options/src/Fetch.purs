module Fetch (fetch) where

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Options (Options, options)
import Fetch.Options (FetchOptions, defaults)
import Node.HTTP (HTTP)
import Prelude (Unit, ($), (<>), (<<<))

foreign import fetchImpl ::
  forall eff.
    Foreign
    -> (String -> Eff (http :: HTTP | eff) Unit)
    -> (String -> Eff (http :: HTTP | eff) Unit)
    -> (Eff (http :: HTTP | eff) Unit)

fetch :: forall eff. Options FetchOptions -> ContT Unit (Eff (http :: HTTP | eff)) (Either String String)
fetch opts = ContT \k ->
  fetchImpl (options $ defaults <> opts) (k <<< Right) (k <<< Left)
