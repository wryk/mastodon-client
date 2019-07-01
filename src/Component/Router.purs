module App.Component.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Component.HTML.Utils (routeHref)
import App.Data.Route (Route(..), routeCodec)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
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
    ()

component
    :: ∀ m
     . MonadAff m
    => Navigate m
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
                [ HH.ul_
                    [ HH.li_
                        [ HH.a
                            [ routeHref Home
                            ]
                            [ HH.text "home"
                            ]
                        ]
                    , HH.li_
                        [ HH.a
                            [ routeHref HomeTimeline
                            ]
                            [ HH.text "home timeline"
                            ]
                        ]
                    , HH.li_
                        [ HH.a
                            [ routeHref PublicTimeline
                            ]
                            [ HH.text "public timeline"
                            ]
                        ]
                    , HH.li_
                        [ HH.a
                            [ routeHref ConversationsTimeline
                            ]
                            [ HH.text "conversations"
                            ]
                        ]
                    , HH.li_
                        [ HH.a
                            [ routeHref (HashtagTimeline "purescript")
                            ]
                            [ HH.text "hashtag #purescript"
                            ]
                        ]
                    , HH.li_
                        [ HH.a
                            [ routeHref (ListTimeline "so")
                            ]
                            [ HH.text "list SO"
                            ]
                        ]
                    ]
                , case route of
                    Just r -> case r of
                        Home ->
                            HH.text "Home"
                        HomeTimeline ->
                            HH.text "HomeTimeline"
                        PublicTimeline ->
                            HH.text "PublicTimeline"
                        ConversationsTimeline ->
                            HH.text "ConversationsTimeline"
                        (HashtagTimeline hastag) ->
                            HH.text "HashtagTimeline"
                        (ListTimeline list) ->
                            HH.text "ListTimeline"
                    Nothing ->
                        HH.text "404"
                ]