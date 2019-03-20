module App.Page.PublicTimeline where

import Prelude

import App.Capability.Navigate (class Navigate)
import Halogen as H
import Halogen.HTML as HH

type State =
    Unit

component :: ∀ q i o m. Navigate m => H.Component HH.HTML q i o m
component =
    H.mkComponent
        { initialState: const unit
        , render
        , eval: H.mkEval H.defaultEval
        }

render :: ∀ m. State -> H.ComponentHTML Unit () m
render _ =
    HH.div_
        [ HH.text "PUBLIC TIMELINE"
        ]