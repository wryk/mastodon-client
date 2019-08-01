module App.AppM where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.Instance (class ManageInstance)
import App.Capability.Resource.Timeline (class ManageTimeline)
import App.Data.Route (routeCodec)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Either (hush)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex as RD
import Routing.Hash as RH
import Simple.Ajax (get)
import Type.Equality (class TypeEquals, from)

type Environment =
    { domain :: String
    }

newtype AppM a = AppM (ReaderT Environment Aff a)

runAppM :: Environment -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Environment => MonadAsk e AppM where
    ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
    navigate = liftEffect <<< RH.setHash <<< RD.print routeCodec

instance manageInstanceAppM :: ManageInstance AppM where
    getInstance = liftAff $ hush <$> get "https://eldritch.cafe/api/v1/instance"

instance manageTimelineAppM :: ManageTimeline AppM where
    getPublic = liftAff $ hush <$> get "https://eldritch.cafe/api/v1/timelines/public"