module App.Type.Attachment where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Attachment = Attachment
	{ id :: String
	-- todo: attachment type
	, type :: String
	, url :: String
	, remoteUrl :: Maybe String
	, previewUrl :: String
	, textUrl :: Maybe String
	-- todo: attachment meta
	-- , meta :: Maybe String
	, description :: Maybe String
	}

derive instance newtypeAttachment :: Newtype Attachment _

derive instance genericAttachment :: Generic Attachment _

instance decodeAttachment :: Decode Attachment where
	decode = foreignGenericDecode

instance encodeAttachment :: Encode Attachment where
	encode = foreignGenericEncode
