module ForeignGeneric where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Console (logShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)

data Entry = Entry { title :: String }
derive instance genericEntry :: Generic Entry _
instance showEntry :: Show Entry where
  -- show = genericShow
  show (Entry e) = e.title

newtype EntryTp = EntryTp { title :: String, date :: String }
newtype EntryTpArray = EntryTpArray (Array EntryTp)
derive instance genericEntryTp :: Generic EntryTp _
derive instance genericEntryTps :: Generic EntryTpArray _
instance decodeEntryTp :: Decode EntryTp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance decodeEntryTpArray :: Decode EntryTpArray where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntryTp :: Encode EntryTp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntryTpArray :: Encode EntryTpArray where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
instance showEntryTp :: Show EntryTp where
  show = encodeJSON
instance showEntryTpArray :: Show EntryTpArray where
  show = encodeJSON


json :: String
json = "[{\"title\":\"title1\",\"date\":\"2017-01-01\"}]"

read :: Effect Unit
read = do
  case runExcept $ decodeJSON json :: _ EntryTpArray of
    Left a -> logShow "Deocde Error!!"
    Right (EntryTpArray ([EntryTp e])) -> do
      logShow $ Entry {title: e.title}
    Right _ -> logShow $ runExcept $ decodeJSON json :: _ EntryTpArray
