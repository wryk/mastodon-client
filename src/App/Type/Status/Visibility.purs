module App.Type.Status.Visibility where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Foreign (ForeignError(..), fail, readString)
import Foreign.Class (class Decode, class Encode, encode)

data Visibility
	= Public
	| Unlisted
	| Private
	| Direct

fromString :: String -> Maybe Visibility
fromString = case _ of
	"public" -> Just Public
	"unlisted" -> Just Unlisted
	"private" -> Just Private
	"direct" -> Just Direct
	_ -> Nothing

toString :: Visibility -> String
toString = case _ of
	Public -> "public"
	Unlisted -> "unlisted"
	Private -> "private"
	Direct -> "direct"

instance decodeVisibility :: Decode Visibility where
	decode value = do
		string <- readString value
		maybe (fail $ ForeignError "Unknow visibility") pure (fromString string)

instance encodeVisibility :: Encode Visibility where
	encode = encode <<< toString
