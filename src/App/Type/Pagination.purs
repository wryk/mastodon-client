module App.Type.Pagination where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)

import App.Common (foreignGenericDecode, foreignGenericEncode)

newtype Pagination = Pagination
	{ maxId :: Maybe String
	, sinceId :: Maybe String
	, limit :: Maybe Int
	}

derive instance newtypePagination :: Newtype Pagination _

derive instance genericPagination :: Generic Pagination _

instance decodePagination :: Decode Pagination where
	decode = foreignGenericDecode

instance encodePagination :: Encode Pagination where
	encode = foreignGenericEncode
