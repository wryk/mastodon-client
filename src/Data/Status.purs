module App.Data.Status where

import App.Data.Account
import Data.Maybe (Maybe)
-- import Simple.JSON (class ReadForeign, class WriteForeign)

type Status =
    { id :: String
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

newtype RebloggedStatus
    = RebloggedStatus Status

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