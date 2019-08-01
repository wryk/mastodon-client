module App.Data.Status where

import App.Data.Account
import Data.Either (Either)
import Data.Maybe (Maybe)
import Foreign (MultipleErrors)
import Simple.JSON (readJSON)

type Status =
    { id :: String
    , uri :: String
    , url :: Maybe String
    , account :: Account
    , content :: String
    }

readStatus :: String -> Either MultipleErrors Status
readStatus = readJSON