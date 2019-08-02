module App.Page.Home where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.Instance (class ManageInstance, getInstance)
import App.Component.RawHTML as RawHTML
import App.Data.Instance (Instance)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromMaybe)

type State =
    { server :: RemoteData String Instance
    }

data Action
    = Initialize

type ChildSlots =
    ( "description" :: H.Slot (Const Void) Void Unit
    )

component
    :: ∀ m
    . MonadAff m
    => Navigate m
    => ManageInstance m
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
            { server: NotAsked
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
              H.modify_ _ { server = Loading }
              server <- getInstance
              H.modify_ _ { server = fromMaybe server }

        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text "HOME" ]
                , (renderInstanceData state.server)
                ]

        -- renderInstanceData :: ∀ props. RemoteData String Instance -> HH.HTML props Action
        renderInstanceData = case _ of
            NotAsked ->
                HH.div_
                    [ HH.text "Your instance data"
                    ]
            Loading ->
                HH.div_
                    [ HH.text "Loading ..."
                    ]
            Failure error ->
                HH.div_
                    [ HH.text "Error :/"
                    ]
            Success server ->
                renderInstance server

        -- renderInstance :: ∀ props. Instance -> HH.HTML props Action
        renderInstance server =
            HH.div_
                [ HH.h2_ [ HH.text server.title ]
                -- , HH.p_ [ HH.text server.description ]
                , HH.p_
                    [ HH.slot (SProxy :: _ "description") unit RawHTML.component server.description absurd
                    ]
                , renderThumbnail server
                ]

        -- renderThumbnail :: ∀ props. Instance -> HH.HTML props Action
        renderThumbnail server = case server.thumbnail of
            Nothing ->
                HH.text ""
            Just thumbnail ->
                HH.img
                    [ HP.src thumbnail
                    , HP.alt $ server.title <> " thumbnail"
                    , HP.width 400
                    ]