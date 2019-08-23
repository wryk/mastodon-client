module App.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Resource.Account (class ManageAccount)
import App.Capability.Resource.Instance (class ManageInstance)
import App.Capability.Resource.Timeline (class ManageTimeline)
import App.Capability.Resource.Status (class ManageStatus)
import App.Data.Route (Route(..), routeCodec)
import App.Page.Account as Account
import App.Page.Status as Status
import App.Page.PublicTimeline as PublicTimeline
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Routing.Hash as RH

type State =
    { route :: Maybe Route
    }

data Action
    = Initialize

data Query a
    = Navigate Route a

type OpaqueSlot = H.Slot (Const Void) Void

type ChildSlots =
    ( home :: OpaqueSlot Unit
    , account :: OpaqueSlot Unit
    , status :: OpaqueSlot Unit
    , publicTimeline :: OpaqueSlot Unit
    )

component
    :: ∀ m
     . MonadAff m
    => Navigate m
    => ManageAccount m
    => ManageInstance m
    => ManageTimeline m
    => ManageStatus m
    => H.Component HH.HTML Query Unit Void m
component = H.mkComponent
    { initialState: \_ -> { route: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    }

    where
        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect RH.getHash
                navigate $ fromMaybe Home initialRoute

        handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
        handleQuery = case _ of
            Navigate destination a -> do
                { route } <- H.get

                when (route /= Just destination) do
                    H.modify_ _ { route = Just destination }

                pure $ Just a

        render :: State -> H.ComponentHTML Action ChildSlots m
        render { route } =
            HH.main_
                [ case route of
                    Just r -> case r of
                        Home ->
                            HH.slot (SProxy :: _ "home") unit PublicTimeline.component unit absurd
                        (Account accountId) ->
                            HH.slot (SProxy :: _ "account") unit Account.component accountId absurd
                        (Status statusId) ->
                            HH.slot (SProxy :: _ "status") unit Status.component statusId absurd
                        _ ->
                            HH.text "YADA YADA YADA"
                    Nothing ->
                        HH.text "404"
                ]