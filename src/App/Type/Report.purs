module App.Type.Report where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Report = Report
	{ id :: String
	, actionTaken :: Boolean
	}

derive instance newtypeReport :: Newtype Report _

derive instance genericReport :: Generic Report _

instance decodeReport :: Decode Report where
	decode = foreignGenericDecode

instance encodeReport :: Encode Report where
	encode = foreignGenericEncode
