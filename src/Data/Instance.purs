module App.Data.Instance where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Foreign (MultipleErrors)
import Simple.JSON (readJSON)

type Instance =
    { uri :: String
    , title :: String
    , description :: String
    , email :: String
    , version :: String
    , thumbnail :: Maybe String
    }

readInstance :: String -> Either MultipleErrors Instance
readInstance = readJSON

decodeInstance :: Json -> Either String Instance
decodeInstance json = do
    obj <- decodeJson json
    uri <- obj .: "uri"
    title <- obj .: "title"
    description <- obj .: "description"
    email <- obj .: "email"
    version <- obj .: "version"
    thumbnail <- obj .:? "thumbnail"
    pure { uri, title, description, email, version, thumbnail }