module App.Type.Error where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Error = Error
	{ error :: String
	}

derive instance newtypeError :: Newtype Error _

derive instance genericError :: Generic Error _

instance decodeError :: Decode Error where
	decode = foreignGenericDecode

instance encodeError :: Encode Error where
	encode = foreignGenericEncode
