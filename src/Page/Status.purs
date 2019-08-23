module App.Page.Status where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.Status (class ManageStatus, getStatus, getStatusContext)
import App.Component.Status as StatusComponent
import App.Data.Status (Context, Status, StatusId)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..), fromMaybe)

type State =
    { statusId :: StatusId
    , status :: RemoteData String Status
    , context :: RemoteData String Context
    }

data Action
    = Initialize

type ChildSlots =
    ( "status" :: H.Slot (Const Void) Void StatusId
    )

_status = SProxy :: SProxy "status"

component
    :: ∀ m
    . MonadAff m
    => Navigate m
    => ManageStatus m
    => H.Component HH.HTML (Const Void) StatusId Void m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

    where
        initialState :: StatusId -> State
        initialState statusId =
            { statusId
            , status: NotAsked
            , context: NotAsked
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                statusId <- H.gets _.statusId

                H.modify_ _ { status = Loading }
                status <- getStatus statusId
                H.modify_ _ { status = fromMaybe status }

                H.modify_ _ { context = Loading }
                context <- getStatusContext statusId
                H.modify_ _ { context = fromMaybe context }

        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text "Status" ]
                , renderContextAncestorsData state.context
                , HH.div_
                    [ renderStatusData state.status ]
                , renderContextDescendantsData state.context
                ]

        renderContextAncestorsData :: RemoteData String Context -> H.ComponentHTML Action ChildSlots m
        renderContextAncestorsData = case _ of
            NotAsked ->
                HH.div_
                    [ HH.text "NOT ASKED"
                    ]
            Loading ->
                HH.div_
                    [ HH.text "Loading ..."
                    ]
            Failure error ->
                HH.div_
                    [ HH.text "Error :/"
                    ]
            Success context ->
                HH.div_
                    $ context.ancestors <#> \status ->
                        HH.slot _status status.id StatusComponent.component status absurd

        renderContextDescendantsData :: RemoteData String Context -> H.ComponentHTML Action ChildSlots m
        renderContextDescendantsData = case _ of
            NotAsked ->
                HH.div_
                    [ HH.text "NOT ASKED"
                    ]
            Loading ->
                HH.div_
                    [ HH.text "Loading ..."
                    ]
            Failure error ->
                HH.div_
                    [ HH.text "Error :/"
                    ]
            Success context ->
                HH.div_
                    $ context.descendants <#> \status ->
                        HH.slot _status status.id StatusComponent.component status absurd

        renderStatusData :: RemoteData String Status -> H.ComponentHTML Action ChildSlots m
        renderStatusData = case _ of
            NotAsked ->
                HH.div_
                    [ HH.text "NOT ASKED"
                    ]
            Loading ->
                HH.div_
                    [ HH.text "Loading ..."
                    ]
            Failure error ->
                HH.div_
                    [ HH.text "Error :/"
                    ]
            Success status ->
                HH.div_
                    [ HH.slot _status status.id StatusComponent.component status absurd
                    ]