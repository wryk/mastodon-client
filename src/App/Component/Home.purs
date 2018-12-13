module App.Component.Home where

import Prelude

import App.Component.Timeline as TimelineComponent
import App.Component.Timeline (Message(..)) as TimelineComponentMessage
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Array String

data Query a
	= Initialize a
	| Finalize a
	| HandleTimelineMessage TimelineId TimelineComponent.Message a

type Input = Unit

data Message
	= Initialized
	| Finalized

type TimelineId = String
data TimelineSlot = TimelineSlot TimelineId
derive instance eqTimelineSlot :: Eq TimelineSlot
derive instance ordTimelineSlot :: Ord TimelineSlot

component :: H.Component HH.HTML Query Input Message Aff
component =
	H.lifecycleParentComponent
		{ initialState: const initialState
		, render
		, eval
		, receiver: const Nothing
		, initializer: Just $ H.action Initialize
		, finalizer: Just $ H.action Finalize
		}

	where
		initialState :: State
		initialState =
			[ "eldritch.cafe"
			, "catboy.cafe"
			, "catgirl.science"
			, "social.mochi.academy"
			, "pipou.academy"
			]

		render :: State -> H.ParentHTML Query TimelineComponent.Query TimelineSlot Aff
		render state =
			HH.main
				[ HP.class_ $ HH.ClassName "app"
				]
				(map renderTimeline state)

		renderTimeline :: String -> H.ParentHTML Query TimelineComponent.Query TimelineSlot Aff
		renderTimeline domain =
			HH.slot
				(TimelineSlot domain)
				TimelineComponent.component
				domain
				(HE.input (HandleTimelineMessage domain))


		eval :: Query ~> H.ParentDSL State Query TimelineComponent.Query TimelineSlot Message Aff
		eval = case _ of
			Initialize next -> do
				H.liftEffect $ log "Home: Initialize"
				H.raise Initialized
				pure next

			Finalize next -> do
				H.liftEffect $ log "Home: Finalize"
				H.raise Finalized
				pure next

			HandleTimelineMessage timelineId message next -> do
				case message of
					TimelineComponentMessage.Initialized -> do
						H.liftEffect $ log $ ("Home: Timeline " <> timelineId <> " initialized")

					TimelineComponentMessage.Finalized -> do
						H.liftEffect $ log $ ("Home: Timeline " <> timelineId <> " finalized")

				pure next
