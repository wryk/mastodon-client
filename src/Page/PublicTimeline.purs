module App.Page.PublicTimeline where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.Timeline (class ManageTimeline, getPublic)
import App.Data.Status (Status)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..), fromMaybe)

type State =
    { statuses :: RemoteData String (Array Status)
    }

data Action
    = Initialize

component
    :: ∀ m
    . Navigate m
    => ManageTimeline m
    => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

    where
        initialState :: Unit -> State
        initialState _ =
            { statuses: NotAsked
            }

        handleAction :: Action -> H.HalogenM State Action () Void m Unit
        handleAction = case _ of
            Initialize -> do
              H.modify_ _ { statuses = Loading }
              statuses <- getPublic
              H.modify_ _ { statuses = fromMaybe statuses }

        render :: State -> H.ComponentHTML Action () m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text "PUBLIC TIMELINE" ]
                , (renderStatusesData state.statuses)
                ]

        renderStatusesData :: ∀ props. RemoteData String (Array Status) -> HH.HTML props Action
        renderStatusesData = case _ of
            NotAsked ->
                HH.div_
                    [ HH.text "Your statuses data"
                    ]
            Loading ->
                HH.div_
                    [ HH.text "Loading ..."
                    ]
            Failure error ->
                HH.div_
                    [ HH.text "Error :/"
                    ]
            Success statuses ->
                renderStatuses statuses

        renderStatuses :: ∀ props. (Array Status) -> HH.HTML props Action
        renderStatuses statuses =
            HH.div_
                (statuses <#> renderStatus)


        renderStatus ::  ∀ props. Status -> HH.HTML props Action
        renderStatus status =
            HH.div_
                [ HH.h2_ [ HH.text status.uri ]
                , HH.p_ [ HH.text status.content ]
                ]