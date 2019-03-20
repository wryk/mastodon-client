module App.Component.HTML.Utils where

import Prelude

import App.Data.Route (Route, routeCodec)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

routeHref :: ∀ r i. Route -> HH.IProp ( href :: String | r) i
routeHref = HP.href <<< append "#" <<< print routeCodec