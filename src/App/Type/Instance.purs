module App.Type.Instance where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)
import App.Type.Account (Account)

newtype Instance = Instance
	{ uri :: String
	, title :: String
	, description :: String
	, email :: String
	, version :: String
	, urls :: Array String
	, languages :: Array String
	, contactAccount :: Account
	}

derive instance newtypeInstance :: Newtype Instance _

derive instance genericInstance :: Generic Instance _

instance decodeInstance :: Decode Instance where
	decode = foreignGenericDecode

instance encodeInstance :: Encode Instance where
	encode = foreignGenericEncode
