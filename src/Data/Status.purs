module App.Data.Status where

import App.Data.Account
import Data.Maybe (Maybe)

type Status =
    { id :: String
    , uri :: String
    , url :: Maybe String
    , account :: Account
    , content :: String
    , created_at :: String
    }