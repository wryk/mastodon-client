module App.Component.Home where

import Prelude

import App.API.Client (fetchPublicTimeline)
import App.Component.Status as StatusComponent
import App.Component.Status (Message(..)) as StatusComponentMessage
import App.Type.Status (Status(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
	{ domain :: String
	, statuses :: Array Status
	}

data Query a
	= Initialize a
	| Finalize a
	| HandleStatusMessage StatusId StatusComponent.Message a

type Input = Unit

data Message
	= Initialized
	| Finalized

type StatusId = String
data StatusSlot = StatusSlot StatusId
derive instance eqStatusSlot :: Eq StatusSlot
derive instance ordStatusSlot :: Ord StatusSlot

component :: H.Component HH.HTML Query Input Message Aff
component =
	H.lifecycleParentComponent
		{ initialState: const initialState
		, render
		, eval
		, initializer: Just $ H.action Initialize
		, finalizer: Just $ H.action Finalize
		, receiver: const Nothing
		}

	where
		initialState :: State
		initialState =
			{ domain: "eldritch.cafe"
			, statuses: []
			}

		render :: State -> H.ParentHTML Query StatusComponent.Query StatusSlot Aff
		render state =
			HH.main
				[ HP.class_ $ HH.ClassName "app"
				]
				[ HH.h1_
					[ HH.text $ "[Federated] " <> state.domain
					]
				, HH.ul
					[ HP.class_ $ HH.ClassName "status-list"
					]
					(map renderStatus state.statuses)
				]

		renderStatus :: Status -> H.ParentHTML Query StatusComponent.Query StatusSlot Aff
		renderStatus status@(Status r) =
			HH.li
				[ HP.class_ $ HH.ClassName "status-list-item"
				]
				[ HH.slot
					(StatusSlot r.id)
					(StatusComponent.component status)
					unit
					(HE.input (HandleStatusMessage r.id))
				]


		eval :: Query ~> H.ParentDSL State Query StatusComponent.Query StatusSlot Message Aff
		eval = case _ of
			Initialize next -> do
				H.liftEffect $ log "Home: Initialize"

				domain <- H.gets _.domain
				H.liftEffect $ log $ "Domain is " <> domain

				response <- H.liftAff $ fetchPublicTimeline domain

				case response of
					Left error ->
						H.liftEffect $ log "network error"

					Right content ->
						case content of
							Left errors ->
								H.liftEffect $ log $ "decode error : " <> show errors

							Right statuses ->
								H.modify_ $ _ { statuses = statuses }

				H.raise Initialized
				pure next

			Finalize next -> do
				H.liftEffect $ log "Home: Finalize"
				H.raise Finalized
				pure next

			HandleStatusMessage statusId message next -> do
				case message of
					StatusComponentMessage.Initialized -> do
						H.liftEffect $ log $ ("Home: Status " <> statusId <> " initialized")

					StatusComponentMessage.Finalized -> do
						H.liftEffect $ log $ ("Home: Status " <> statusId <> " finalized")

				pure next
