module App.Type.Card where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Card = Card
	{ name :: String
	, title :: String
	, description :: String
	, image :: Maybe String
	-- todo: card type
	, type :: String
	-- todo: inspect what is omembed data for the followings props
	, authorName :: Maybe String
	, authorUrl :: Maybe String
	, providerName :: Maybe String
	, providerUrl :: Maybe String
	, html :: Maybe String
	, width :: Maybe String
	, height :: Maybe String
	}

derive instance newtypeCard :: Newtype Card _

derive instance genericCard :: Generic Card _

instance decodeCard :: Decode Card where
	decode = foreignGenericDecode

instance encodeCard :: Encode Card where
	encode = foreignGenericEncode
