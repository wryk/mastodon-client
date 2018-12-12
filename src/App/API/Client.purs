module App.API.Client where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON)
import Milkis (URL(..), defaultFetchOptions, fetch, text)
import Milkis.Impl.Window (windowFetch)

import App.Type.Context (Context)
import App.Type.Card (Card)
import App.Type.Emoji (Emoji)
import App.Type.Instance (Instance)
import App.Type.Status (Status)

type Domain = String
type Token = String
type Resource = String

type R a = Aff (Either Error (Either MultipleErrors a))

request :: ∀ a. Decode a => String -> R a
request url =
	attempt $ (fetch windowFetch) (URL url) defaultFetchOptions >>= text <#> decodeJSON >>> runExcept

fetchInstance :: Domain -> R Instance
fetchInstance domain =
	request $ "https://" <> domain <> "/api/v1/instance"

fetchCustomEmojis :: Domain -> R (Array Emoji)
fetchCustomEmojis domain =
	request $ "https://" <> domain <> "/api/v1/custom_emojis"

fetchStatus :: Domain -> String -> R Status
fetchStatus domain id =
	request $ "https://" <> domain <> "/api/v1/statuses/" <> id

fetchStatusContext :: Domain -> String -> R Context
fetchStatusContext domain id =
	request $ "https://" <> domain <> "/api/v1/statuses/" <> id <> "/context"

fetchStatusCard :: Domain -> String -> R Card
fetchStatusCard domain id =
	request $ "https://" <> domain <> "/api/v1/statuses/" <> id <> "/card"

fetchStatusRebloggedBy :: Domain -> String -> R Card
fetchStatusRebloggedBy domain id =
	request $ "https://" <> domain <> "/api/v1/statuses/" <> id <> "/reblogged_by"

fetchStatusFavouritedBy :: Domain -> String -> R Card
fetchStatusFavouritedBy domain id =
	request $ "https://" <> domain <> "/api/v1/statuses/" <> id <> "/favourited_by"

fetchPublicTimeline :: Domain -> R (Array Status)
fetchPublicTimeline domain =
	request $ "https://" <> domain <> "/api/v1/timelines/public"

fetchTagTimeline :: Domain -> String -> R (Array Status)
fetchTagTimeline domain hashtag =
	request $ "https://" <> domain <> "/api/v1/timelines/" <> hashtag
