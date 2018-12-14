module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import App.Component.App (component)

main :: Effect Unit
main = HA.runHalogenAff do
	body <- HA.awaitBody
	runUI component unit body
