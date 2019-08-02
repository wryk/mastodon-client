module App.Data.Instance where

import Data.Maybe (Maybe)

type Instance =
    { uri :: String
    , title :: String
    , description :: String
    , email :: String
    , version :: String
    , thumbnail :: Maybe String
    }