module App.Data.Account where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Foreign (MultipleErrors)
import Simple.JSON (readJSON)

type Account =
    { id :: String
    , username :: String
    , acct :: Maybe String
    , display_name :: String
    }

readAccount :: String -> Either MultipleErrors Account
readAccount = readJSON