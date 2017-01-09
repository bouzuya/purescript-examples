module Fetch.Options
  ( FetchOptions
  , Method(..)
  , defaults
  , headers
  , method
  , url
  ) where

import Prelude (show)
import Data.Show (class Show)
import Data.Functor.Contravariant (cmap)
import Data.Options (Option, Options, defaultToOptions, options, opt)
import Data.StrMap (StrMap)

data Method = DELETE | GET | HEAD | PATCH | POST | PUT

instance showMethod :: Show Method where
  show DELETE = "DELETE"
  show GET = "GET"
  show HEAD = "HEAD"
  show PATCH = "PATCH"
  show POST = "POST"
  show PUT = "PUT"

data FetchOptions

defaults :: Options FetchOptions
defaults = defaultToOptions "method" GET

headers :: Option FetchOptions (StrMap String)
headers = opt "headers"

method :: Option FetchOptions Method
method = cmap show (opt "method")

url :: Option FetchOptions String
url = opt "url"
