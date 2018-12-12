module App.Component.Status where

import Prelude

import App.Type.Status (Status)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
	{ status :: Status
	}

data Query a
	= Initialize a
	| Finalize a

type Input = Unit

data Message
	= Initialized
	| Finalized

component :: Status -> H.Component HH.HTML Query Input Message Aff
component initialStatus =
	H.lifecycleComponent
		{ initialState: const $ { status: initialStatus }
		, render
		, eval
		, initializer: Just $ H.action Initialize
		, finalizer: Just $ H.action Finalize
		, receiver: const Nothing
		}

	where
		render :: State -> H.ComponentHTML Query
		render state =
			let
				status = unwrap state.status
				account = unwrap status.account
			in
				HH.div
					[ HP.class_ $ HH.ClassName "status"
					]
					[ HH.h2_
						[ HH.text account.username
						]
					, HH.p_
						[ HH.text status.content
						]
					, HH.a
						[ HP.href status.uri
						]
						[ HH.text status.uri
						]
					, HH.div_
						[ HH.button
							[ HP.type_ HP.ButtonButton
							]
							[ HH.text "REPLY"
							]
						, HH.button
							[ HP.type_ HP.ButtonButton
							]
							[ HH.text "FAVOURITE"
							]
						, HH.button
							[ HP.type_ HP.ButtonButton
							]
							[ HH.text "REBLOG"
							]
						, HH.button
							[ HP.type_ HP.ButtonButton
							]
							[ HH.text "MORE"
							]
						]
					]

		eval :: Query ~> H.ComponentDSL State Query Message Aff
		eval = case _ of
			Initialize next -> do
				H.liftEffect $ log "Status: Initialize"
				H.raise Initialized
				pure next

			Finalize next -> do
				H.liftEffect $ log "Status: Finalize"
				H.raise Finalized
				pure next
