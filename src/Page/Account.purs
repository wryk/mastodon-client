module App.Page.Account where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.Account (class ManageAccount, getAccount)
import App.Component.RawHTML as RawHTML
import App.Data.Account (Account, AccountId)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..), fromMaybe)

type State =
    { accountId :: AccountId
    , account :: RemoteData String Account
    }

data Action
    = Initialize

type ChildSlots =
    ( "note" :: H.Slot (Const Void) Void Unit
    )

_note = SProxy :: SProxy "note"

component
    :: ∀ m
    . MonadAff m
    => Navigate m
    => ManageAccount m
    => H.Component HH.HTML (Const Void) AccountId Void m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

    where
        initialState :: AccountId -> State
        initialState accountId =
            { accountId
            , account: NotAsked
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                H.modify_ _ { account = Loading }
                accountId <- H.gets _.accountId
                account <- getAccount accountId
                H.modify_ _ { account = fromMaybe account }

        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text "Account" ]
                , renderAccountData $ state.account
                ]

        renderAccountData :: RemoteData String Account -> H.ComponentHTML Action ChildSlots m
        renderAccountData = case _ of
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
            Success account ->
                HH.div_
                    [ HH.text account.display_name
                    , HH.slot _note unit RawHTML.component account.note absurd
                    ]