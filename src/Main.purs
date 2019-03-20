module Main where

import Prelude

import App.AppM (Environment,runAppM)
import App.Component.Router as Router
import App.Data.Route (Route, routeCodec)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (getHash, matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
	body <- HA.awaitBody

	initialHash <- liftEffect $ getHash

	let
		environment :: Environment
		environment =
			{ domain: "eldritch.cafe"
			}

		rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
		rootComponent = H.hoist (runAppM environment) Router.component

		initialRoute :: Maybe Route
		initialRoute = hush $ parse routeCodec initialHash

	halogenIO <- runUI rootComponent initialRoute body

	void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
		when (old /= Just new) do
		launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new