module Fetch.Options
  ( FetchOption
  , FetchOptions
  , FetchOption'
  , Method(..)
  , headers
  , method
  , url
  ) where

import Prelude (show)
import Data.Show (class Show)
import Data.Functor.Contravariant (cmap)
import Data.Options (Option, Options, options, opt) as Options
import Data.StrMap (StrMap)

data Method = DELETE | GET | HEAD | PATCH | POST | PUT

instance showMethod :: Show Method where
  show DELETE = "DELETE"
  show GET = "GET"
  show HEAD = "HEAD"
  show PATCH = "PATCH"
  show POST = "POST"
  show PUT = "PUT"

data FetchOption'

type FetchOption a = Options.Option FetchOption' a
type FetchOptions = Options.Options FetchOption'

headers :: FetchOption (StrMap String)
headers = Options.opt "headers"

method :: FetchOption Method
method = cmap show (Options.opt "method")

url :: FetchOption String
url = Options.opt "url"
