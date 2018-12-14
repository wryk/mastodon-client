module App.Component.Timeline where

import Prelude

import App.API.Client (fetchPublicTimeline)
import App.Component.Status (Message(..)) as StatusComponentMessage
import App.Component.Status as StatusComponent
import App.Type.Status (Status(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input = String

type State =
	{ domain :: String
	, statuses :: Array Status
	}

data Query a
	= HandleInput String a
	| Initialize a
	| Finalize a
	| HandleStatusMessage StatusId StatusComponent.Message a

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
		{ initialState
		, render
		, eval
		, receiver: HE.input HandleInput
		, initializer: Just $ H.action Initialize
		, finalizer: Just $ H.action Finalize
		}

	where
		initialState :: Input -> State
		initialState domain =
			{ domain: domain
			, statuses: []
			}

		render :: State -> H.ParentHTML Query StatusComponent.Query StatusSlot Aff
		render state =
			HH.section
				[ HP.class_ $ HH.ClassName "timeline"
				]
				[ HH.header
					[ HP.class_ $ HH.ClassName "timeline-title"
					]
					[ HH.text $ state.domain
					]
				, HH.div
					[ HP.class_ $ HH.ClassName "timeline-statuses"
					]
					(map renderStatus state.statuses)
				]

		renderStatus :: Status -> H.ParentHTML Query StatusComponent.Query StatusSlot Aff
		renderStatus status@(Status r) =
			HH.div
				[ HP.class_ $ HH.ClassName "timeline-status"
				]
				[ HH.slot
					(StatusSlot r.id)
					(StatusComponent.component status)
					unit
					(HE.input (HandleStatusMessage r.id))
				]


		eval :: Query ~> H.ParentDSL State Query StatusComponent.Query StatusSlot Message Aff
		eval = case _ of
			HandleInput domain next -> do
				state <- H.get
				when (state.domain /= domain) $ H.put $ initialState domain
				pure next

			Initialize next -> do
				H.liftEffect $ log "Timeline: Initialize"

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
				H.liftEffect $ log "Timeline: Finalize"
				H.raise Finalized
				pure next

			HandleStatusMessage statusId message next -> do
				case message of
					StatusComponentMessage.Initialized -> do
						H.liftEffect $ log $ ("Timeline: Status " <> statusId <> " initialized")

					StatusComponentMessage.Finalized -> do
						H.liftEffect $ log $ ("Timeline: Status " <> statusId <> " finalized")

				pure next
