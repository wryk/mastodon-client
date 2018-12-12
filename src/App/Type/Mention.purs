module App.Type.Mention where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Mention = Mention
	{ url :: String
	, username :: String
	, acct :: String
	, id :: String
	}

derive instance newtypeMention :: Newtype Mention _

derive instance genericMention :: Generic Mention _

instance decodeMention :: Decode Mention where
	decode = foreignGenericDecode

instance encodeMention :: Encode Mention where
	encode = foreignGenericEncode
