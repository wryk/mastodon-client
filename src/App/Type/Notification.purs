module App.Type.Notification where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)
import App.Type.Account (Account)
import App.Type.Status (Status)

newtype Notification = Notification
	{ id :: String
	-- todo: notification type
	, type :: String
	-- todo: notification createdAt
	, createdAt :: String
	, account :: Account
	, status :: Maybe Status
	}

derive instance newtypeNotification :: Newtype Notification _

derive instance genericNotification :: Generic Notification _

instance decodeNotification :: Decode Notification where
	decode = foreignGenericDecode

instance encodeNotification :: Encode Notification where
	encode = foreignGenericEncode
