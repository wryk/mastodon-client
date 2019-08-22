module App.Capability.Resource.Account where

import Prelude

import App.Data.Account (Account, AccountId)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageAccount m where
    getAccount :: AccountId -> m (Maybe Account)

instance manageAccountHalogenM :: ManageAccount m => ManageAccount (HalogenM st act cs msg m) where
    getAccount = lift <<< getAccount