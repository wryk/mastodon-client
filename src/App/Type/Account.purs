module App.Type.Account where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)
import App.Type.Emoji (Emoji)

newtype Account = Account
	{ id :: String
	, username :: String
	, acct :: String
	, displayName :: String
	, locked :: Boolean
	, createdAt :: String
	, followersCount :: Int
	, followingCount :: Int
	, statusesCount :: Int
	, note :: String
	, avatar :: String
	, avatarStatic :: String
	, header :: String
	, headerStatic :: String
	, emojis :: Array Emoji
	-- , moved :: Maybe Account
	-- , fields :: Maybe (Array Field)
	, bot :: Maybe Boolean
	}

derive instance newtypeAccount :: Newtype Account _

derive instance genericAccount :: Generic Account _

instance decodeAccount :: Decode Account where
	decode = foreignGenericDecode

instance encodeAccount :: Encode Account where
	encode = foreignGenericEncode

-- type Field =
-- 	{ name :: String
-- 	, value :: String
-- 	}
