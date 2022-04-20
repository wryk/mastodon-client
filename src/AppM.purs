module App.AppM where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.Account (class ManageAccount)
import App.Capability.Resource.Instance (class ManageInstance)
import App.Capability.Resource.Status (class ManageStatus)
import App.Capability.Resource.Timeline (class ManageTimeline)
import App.Data.Account (AccountId(..))
import App.Data.Route (routeCodec)
import App.Data.Status (StatusId(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Either (hush)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex as RD
import Routing.Hash as RH
import Simple.Ajax (get)
import Type.Equality (class TypeEquals, from)

baseApiUrl :: String
baseApiUrl = "https://mastodon.top/api"

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

instance manageAccountAppM :: ManageAccount AppM where
    getAccount (AccountId id) = liftAff $ hush <$> get (baseApiUrl <> "/v1/accounts/" <> id)

instance manageInstanceAppM :: ManageInstance AppM where
    getInstance = liftAff $ hush <$> get (baseApiUrl <> "/v1/instance")

instance manageTimelineAppM :: ManageTimeline AppM where
    getPublic = liftAff $ hush <$> get (baseApiUrl <> "/v1/timelines/public")

instance manageStatusAppM :: ManageStatus AppM where
    getStatus (StatusId id) = liftAff $ hush <$> get (baseApiUrl <> "/v1/statuses/" <> id)
    getStatusContext (StatusId id) = liftAff $ hush <$> get (baseApiUrl <> "/v1/statuses/" <> id <> "/context")
    getStatusFavouritedBy (StatusId id) = liftAff $ hush <$> get (baseApiUrl <> "/v1/statuses/" <> id <> "/favourited_by")
    getStatusRebloggedBy (StatusId id) = liftAff $ hush <$> get (baseApiUrl <> "/v1/statuses/" <> id <> "/reblogged_by")
