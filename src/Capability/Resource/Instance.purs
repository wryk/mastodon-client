module App.Capability.Resource.Instance where

import Prelude

import App.Data.Instance (Instance)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageInstance m where
    getInstance :: m (Maybe Instance)

instance manageInstanceHalogenM :: ManageInstance m => ManageInstance (HalogenM st act cs msg m) where
    getInstance = lift getInstance