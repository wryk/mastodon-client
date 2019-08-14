module App.Data.Status where

import Prelude

import App.Data.Account (Account)
import Data.Maybe (Maybe)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype StatusId = StatusId String

derive newtype instance eqStatusId :: Eq StatusId
derive newtype instance ordStatusId :: Ord StatusId
derive newtype instance readForeignStatusId :: ReadForeign StatusId
derive newtype instance writeForeignStatusId :: WriteForeign StatusId

type Status =
    { id :: StatusId
    , uri :: String
    , url :: Maybe String
    , account :: Account
    , in_reply_to_id :: Maybe String
    , in_reply_to_account_id :: Maybe String
    -- , reblog :: Maybe RebloggedStatus
    , content :: String
    , created_at :: String
    -- , emojis :: Array Emoji
    , replies_count :: Int
    , reblogs_count :: Int
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
    }

-- newtype RebloggedStatus = RebloggedStatus Status

-- derive newtype instance eqRebloggedStatus :: Eq RebloggedStatus
-- derive newtype instance ordRebloggedStatus :: Ord RebloggedStatus
-- derive newtype instance readForeignRebloggedStatus :: ReadForeign RebloggedStatus
-- derive newtype instance writeForeignRebloggedStatus :: WriteForeign RebloggedStatus

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