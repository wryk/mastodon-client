module App.Type.Tag where

import Data.Generic.Rep (class Generic)
-- import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Tag = Tag
	{ name :: String
	, url :: String
	-- todo: tag history
	-- , history :: Maybe History
	}
derive instance newtypeTag :: Newtype Tag _

derive instance genericTag :: Generic Tag _

instance decodeTag :: Decode Tag where
	decode = foreignGenericDecode

instance encodeTag :: Encode Tag where
	encode = foreignGenericEncode

-- type History =
-- 	{ day :: Int
-- 	, uses :: Int
-- 	, accounts :: Int
-- 	}
