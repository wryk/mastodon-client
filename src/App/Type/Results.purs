module App.Type.Results where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)
import App.Type.Account (Account)
import App.Type.Status (Status)

newtype Results = Results
	{ accounts :: Array Account
	, statuses :: Array Status
	, hashtags :: Array String
	}

derive instance newtypeResults :: Newtype Results _

derive instance genericResults :: Generic Results _

instance decodeResults :: Decode Results where
	decode = foreignGenericDecode

instance encodeResults :: Encode Results where
	encode = foreignGenericEncode
