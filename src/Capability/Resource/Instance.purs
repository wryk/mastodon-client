module App.Capability.Resource.Instance where

import Prelude

import App.Data.Instance (Instance)
import Data.Maybe (Maybe)

class Monad m <= ManageInstance m where
    getInstance :: m (Maybe Instance)