module App.Type.List where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype List = List
	{ id :: String
	, title :: String
	}

derive instance newtypeList :: Newtype List _

derive instance genericList :: Generic List _

instance decodeList :: Decode List where
	decode = foreignGenericDecode

instance encodeList :: Encode List where
	encode = foreignGenericEncode
