module App.Type.Context where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)
import App.Type.Status (Status)

newtype Context = Context
	{ ancestors :: Array Status
	, descendants :: Array Status
	}

derive instance newtypeContext :: Newtype Context _

derive instance genericContext :: Generic Context _

instance decodeContext :: Decode Context where
	decode = foreignGenericDecode

instance encodeContext :: Encode Context where
	encode = foreignGenericEncode
