module App.Component.Router where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Component.HTML.Utils (routeHref)
import App.Data.Route (Route(..))
import App.Page.Home as Home
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH

type Input =
    Maybe Route

type State =
    { route :: Route
    }

data Query a =
    Navigate Route a

component :: ∀ m. Navigate m => H.Component HH.HTML Query Input Void m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleQuery = handleQuery
            }
        }

initialState :: Input -> State
initialState initialRoute =
    { route: fromMaybe Home initialRoute
    }

handleQuery :: ∀ act o m a. Query a -> H.HalogenM State act () o m (Maybe a)
handleQuery = case _ of
    Navigate destination a -> do
        state <- H.get

        when (state.route /= destination) do
            H.modify_ _ { route = destination }

        pure $ Just a

render :: ∀ m. State -> H.ComponentHTML Unit () m
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
            ]
        , case route of
            Home ->
                HH.text "Home"
            HomeTimeline ->
                HH.text "HomeTimeline"
            PublicTimeline ->
                HH.text "PublicTimeline"
            _ ->
                HH.text "catchall"
        ]