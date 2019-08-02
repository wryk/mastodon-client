module App.Data.Account where

import Data.Maybe (Maybe)

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