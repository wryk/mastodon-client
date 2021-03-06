module App.Data.Status where

import Prelude

import App.Data.Account (Account)
import Data.Maybe (Maybe)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype StatusId = StatusId String

derive newtype instance showStatusId :: Show StatusId
derive newtype instance eqStatusId :: Eq StatusId
derive newtype instance ordStatusId :: Ord StatusId
derive newtype instance readForeignStatusId :: ReadForeign StatusId
derive newtype instance writeForeignStatusId :: WriteForeign StatusId

type StatusRep row =
    ( id :: StatusId
    , uri :: String
    , url :: Maybe String
    , account :: Account
    , in_reply_to_id :: Maybe StatusId
    , in_reply_to_account_id :: Maybe String
    , content :: String
    , created_at :: String
    -- , emojis :: Array Emoji
    , replies_count :: Int
    , reblogs_count :: Int
    , favourites_count :: Int
    , reblogged :: Maybe Boolean
    , favorited :: Maybe Boolean
    , muted :: Maybe Boolean
    , sensitive :: Boolean
    , spoiler_text :: String
    , visibility :: String
    -- , media_attachments :: Array Attachment
    -- , mentions :: Array Mention
    -- , tags :: Array Tag
    -- , card :: Maybe Card
    -- , poll :: Maybe Poll
    -- , application :: Application
    -- language :: SMaybe tring
    , pinned :: Maybe Boolean
    | row
    )

type RebloggedStatus = { | StatusRep () }

type Status = { | StatusRep (reblog :: Maybe RebloggedStatus) }

type Context =
    { ancestors :: Array Status
    , descendants :: Array Status
    }

data Visibility
    = Public
    | Unlisted
    | Private
    | Direct

-- instance readForeignVisibility :: ReadForeign Visibility where
--     readImpl = case _ of
--         Public ->
--             "public"
--         Unlisted ->
--             "unlisted"
--         Private ->
--             "private"
--         Direct ->
--             "direct"