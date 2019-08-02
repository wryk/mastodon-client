module App.Data.Account where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Foreign (MultipleErrors)
import Simple.JSON (readJSON)

type Account =
    { id :: String
    , username :: String
    , acct :: String
    , display_name :: String
    , locked :: Boolean
    , created_at :: String
    , followers_count :: Int
    , following_count :: Int
    , statuses_count :: Int
    , note :: String
    , url :: String
    , avatar :: String
    , avatar_static :: String
    , header :: String
    , header_static :: String
    , bot :: Maybe Boolean
    }

readAccount :: String -> Either MultipleErrors Account
readAccount = readJSON