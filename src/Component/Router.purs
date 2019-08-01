module App.Component.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Resource.Instance (class ManageInstance)
import App.Capability.Resource.Timeline (class ManageTimeline)
import App.Component.HTML.Utils (routeHref)
import App.Data.Route (Route(..), routeCodec)
import App.Page.Home as Home
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

type ChildSlots =
    ( home :: (H.Slot (Const Void) Void) Unit
    , publicTimeline :: (H.Slot (Const Void) Void) Unit
    )

component
    :: ∀ m
     . MonadAff m
    => Navigate m
    => ManageInstance m
    => ManageTimeline m
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
            HH.div_
                [ renderMenu
                , case route of
                    Just r -> case r of
                        Home ->
                            HH.slot (SProxy :: _ "home") unit Home.component unit absurd
                        HomeTimeline ->
                            HH.text "HomeTimeline"
                        PublicTimeline ->
                            HH.slot (SProxy :: _ "publicTimeline") unit PublicTimeline.component unit absurd
                        ConversationsTimeline ->
                            HH.text "ConversationsTimeline"
                        (HashtagTimeline hastag) ->
                            HH.text "HashtagTimeline"
                        (ListTimeline list) ->
                            HH.text "ListTimeline"
                    Nothing ->
                        HH.text "404"
                ]

        renderMenu :: ∀ props. HH.HTML props Action
        renderMenu =
            HH.ul_
                [ HH.li_
                    [ HH.a
                        [ routeHref Home
                        ]
                        [ HH.text "Home"
                        ]
                    ]
                , HH.li_
                    [ HH.a
                        [ routeHref HomeTimeline
                        ]
                        [ HH.text "Home timeline"
                        ]
                    ]
                , HH.li_
                    [ HH.a
                        [ routeHref PublicTimeline
                        ]
                        [ HH.text "Public timeline"
                        ]
                    ]
                , HH.li_
                    [ HH.a
                        [ routeHref ConversationsTimeline
                        ]
                        [ HH.text "Conversations"
                        ]
                    ]
                , HH.li_
                    [ HH.a
                        [ routeHref (HashtagTimeline "purescript")
                        ]
                        [ HH.text "Hashtag timeline : #purescript"
                        ]
                    ]
                , HH.li_
                    [ HH.a
                        [ routeHref (ListTimeline "so")
                        ]
                        [ HH.text "List timeline : SO"
                        ]
                    ]
                ]