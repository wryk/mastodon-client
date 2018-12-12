module App.Type.Emoji where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Emoji = Emoji
	{ shortcode :: String
	, url :: String
	, staticUrl :: String
	, visibleInPicker :: Boolean
	}

derive instance newtypeEmoji :: Newtype Emoji _

derive instance genericEmoji :: Generic Emoji _

instance decodeEmoji :: Decode Emoji where
	decode = foreignGenericDecode

instance encodeEmoji :: Encode Emoji where
	encode = foreignGenericEncode
