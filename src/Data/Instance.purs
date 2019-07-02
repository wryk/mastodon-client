module App.Data.Instance where

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