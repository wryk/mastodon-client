module App.Component.Status where

import Prelude

import App.Data.Status (Status)
import App.Component.RawHTML as RawHTML
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = Status

type Input = Status

data Action
    = Receive Input

type ChildSlots =
    ( "content" :: H.Slot (Const Void) Void Unit
    )

_content = SProxy :: SProxy "content"

component :: ∀ m. MonadAff m => H.Component HH.HTML (Const Void) Input Void m
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

        render :: State -> H.ComponentHTML Action ChildSlots m
        render status =
            HH.div_
                [ HH.a
                    [ HP.href status.account.url
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