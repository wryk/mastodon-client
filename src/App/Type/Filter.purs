module App.Type.Filter where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Filter = Filter
	{ id :: String
	, phrase :: String
	-- todo: filter context
	, context :: String
	-- todo: filter expiresAt date
	, expiresAt :: Maybe String
	, irreversible :: Boolean
	, wholeWord :: Boolean
	}

derive instance newtypeFilter :: Newtype Filter _

derive instance genericFilter :: Generic Filter _

instance decodeFilter :: Decode Filter where
	decode = foreignGenericDecode

instance encodeFilter :: Encode Filter where
	encode = foreignGenericEncode
