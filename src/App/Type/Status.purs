module App.Type.Status where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)
import App.Type.Account (Account)
import App.Type.Application (Application)
import App.Type.Attachment (Attachment)
import App.Type.Emoji (Emoji)
import App.Type.Mention (Mention)
import App.Type.Status.Visibility (Visibility)
import App.Type.Tag (Tag)

newtype Status = Status
	{ id :: String
	, uri:: String
	, url :: Maybe String
	, account :: Account
	, inReplyToId :: Maybe String
	, inReplyToAccountId :: Maybe String
	-- , reblog :: Maybe Status
	, content :: String
	, createdAt :: String
	, emojis :: Array Emoji
	, repliesCount :: Int
	, reblogsCount :: Int
	, favouritesCount :: Int
	, reblogged :: Maybe Boolean
	, favourited :: Maybe Boolean
	, muted :: Maybe Boolean
	, sensitive :: Boolean
	, spoilerText :: String
	, visibility :: Visibility
	, mediaAttachments :: Array Attachment
	, mentions :: Array Mention
	, tags :: Array Tag
	, application :: Maybe Application
	, language :: Maybe String
	, pinned :: Maybe Boolean
	}

derive instance newtypeStatus :: Newtype Status _

derive instance genericStatus :: Generic Status _

instance decodeStatus :: Decode Status where
	decode = foreignGenericDecode

instance encodeStatus :: Encode Status where
	encode = foreignGenericEncode
