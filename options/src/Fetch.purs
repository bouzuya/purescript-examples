module Fetch (fetch) where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Data.Either (Either(..))
import Data.Options (Options, options)
import Effect (Effect)
import Fetch.Options (FetchOptions, defaults)
import Foreign (Foreign)

foreign import fetchImpl ::
  Foreign
  -> (String -> Effect Unit)
  -> (String -> Effect Unit)
  -> (Effect Unit)

fetch :: Options FetchOptions -> ContT Unit Effect (Either String String)
fetch opts = ContT \k ->
  fetchImpl (options $ defaults <> opts) (k <<< Right) (k <<< Left)
