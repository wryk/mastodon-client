module App.Data.Application where

import Prelude

import Data.Maybe (Maybe)

type Application =
    { name :: String
    , website :: Maybe String
    }