module App.Capability.Resource.Status where

import Prelude

import App.Data.Account (Account)
import App.Data.Status (Context, Status, StatusId)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageStatus m where
    getStatus :: StatusId -> m (Maybe Status)
    getStatusContext :: StatusId -> m (Maybe Context)
    getStatusFavouritedBy :: StatusId -> m (Maybe (Array Account))
    getStatusRebloggedBy :: StatusId -> m (Maybe (Array Account))

instance manageStatusHalogenM :: ManageStatus m => ManageStatus (HalogenM st act cs msg m) where
    getStatus = lift <<< getStatus
    getStatusContext = lift <<< getStatusContext
    getStatusFavouritedBy = lift <<< getStatusFavouritedBy
    getStatusRebloggedBy = lift <<< getStatusRebloggedBy
