module App.Type.Relationship where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Relationship = Relationship
	{ id :: String
	, following :: Boolean
	, followedBy :: Boolean
	, blocking :: Boolean
	, muting :: Boolean
	, mutingNotifications :: Boolean
	, requested :: Boolean
	, domainBlocking :: Boolean
	, showingReblogs :: Boolean
	}

derive instance newtypeRelationship :: Newtype Relationship _

derive instance genericRelationship :: Generic Relationship _

instance decodeRelationship :: Decode Relationship where
	decode = foreignGenericDecode

instance encodeRelationship :: Encode Relationship where
	encode = foreignGenericEncode
