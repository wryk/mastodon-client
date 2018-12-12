module App.Type.Application where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Application = Application
	{ name :: String
	, website :: Maybe String
	}

derive instance newtypeApplication :: Newtype Application _

derive instance genericApplication :: Generic Application _

instance decodeApplication :: Decode Application where
	decode = foreignGenericDecode

instance encodeApplication :: Encode Application where
	encode = foreignGenericEncode
