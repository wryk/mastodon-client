module App.Component.Status where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Component.RawHTML as RawHTML
import App.Data.Route (Route)
import App.Data.Route as Route
import App.Data.Status (Status, StatusRep)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type State = Status

type Input = Status

data Action
    = Receive Input
    | NavigatePrevent MouseEvent Route

type ChildSlots =
    ( "content" :: H.Slot (Const Void) Void Unit
    )

_content = SProxy :: SProxy "content"

component
    :: ∀ m
    . MonadAff m
    => Navigate m
    => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
    where
        initialState :: Input -> State
        initialState status =
            status

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Receive status -> do
                H.put status

            NavigatePrevent mouseEvent route -> do
                H.liftEffect $ preventDefault $ toEvent mouseEvent
                navigate route

        render :: State -> H.ComponentHTML Action ChildSlots m
        render status =
            HH.div
                [ HP.class_ $ HH.ClassName "status"
                ]
                case status.reblog of
                    Nothing ->
                        [ statusView status
                        ]
                    Just rebloggedStatus ->
                        [ reblogHeaderView status
                        , statusView rebloggedStatus
                        ]

        reblogHeaderView status =
            HH.small_
                [ HH.a
                    [ HP.href status.account.url
                    , HE.onClick \event -> Just $ NavigatePrevent event (Route.Account status.account.id)
                    ]
                    [ HH.text status.account.display_name ]
                , HH.text " "
                , HH.text "boosted"
                ]

        statusView :: ∀ r. { | StatusRep r } -> H.ComponentHTML Action ChildSlots m
        statusView status =
            HH.div_
                [ HH.a
                    [ HP.href status.account.url
                    , HE.onClick \event -> Just $ NavigatePrevent event (Route.Account status.account.id)
                    ]
                    [ HH.img
                        [ HP.src status.account.avatar
                        , HP.alt $ status.account.acct <> " avatar"
                        , HP.width 32
                        ]
                    , HH.text status.account.display_name
                    , HH.text " "
                    , HH.small_ [ HH.text status.account.acct ]
                    ]
                , HH.div_ [ HH.slot _content unit RawHTML.component status.content absurd ]
                , HH.a
                    [ HP.href status.uri
                    ]
                    [ HH.text status.created_at
                    ]
                , HH.div_
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        ]
                        [ HH.text "Favourite" ]
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        ]
                        [ HH.text "Boost" ]
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        ]
                        [ HH.text "Share" ]
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        ]
                        [ HH.text "More" ]
                    ]
                ]