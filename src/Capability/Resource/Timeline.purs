module App.Capability.Resource.Timeline where

import Prelude

import App.Data.Status (Status)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageTimeline m where
    getPublic :: m (Maybe (Array Status))

instance manageInstanceHalogenM :: ManageTimeline m => ManageTimeline (HalogenM st act cs msg m) where
    getPublic = lift getPublic