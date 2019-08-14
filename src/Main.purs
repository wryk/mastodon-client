module Main where

import Prelude

import App.AppM (Environment, runAppM)
import App.Router as Router
import App.Data.Route (routeCodec)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as RD
import Routing.Hash as RH

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody

    let
        environment :: Environment
        environment =
            { domain: "eldritch.cafe"
            }

        rootComponent :: H.Component HH.HTML Router.Query Unit Void Aff
        rootComponent = H.hoist (runAppM environment) Router.component

    halogenIO <- runUI rootComponent unit body

    void $ liftEffect $ RH.matchesWith (RD.parse routeCodec) \old new ->
        when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new