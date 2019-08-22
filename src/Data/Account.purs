module App.Data.Account where

import Prelude

import Data.Maybe (Maybe)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype AccountId = AccountId String

derive newtype instance eqAccountId :: Eq AccountId
derive newtype instance ordAccountId :: Ord AccountId
derive newtype instance showAccountId :: Show AccountId
derive newtype instance readForeignAccountId :: ReadForeign AccountId
derive newtype instance writeForeignAccountId :: WriteForeign AccountId

type Account =
    { id :: AccountId
    , username :: String
    , acct :: String
    , display_name :: String
    , locked :: Boolean
    , created_at :: String
    , followers_count :: Int
    , following_count :: Int
    , statuses_count :: Int
    , note :: String
    , url :: String
    , avatar :: String
    , avatar_static :: String
    , header :: String
    , header_static :: String
    -- , emojis :: Array Emoji
    -- , moved :: Maybe MovedAccount
    , fields :: Array Field
    , bot :: Maybe Boolean
    }

newtype MovedAccount
    = MovedAccount Account

type Field =
    { name :: String
    , value :: String
    , verified_at :: Maybe String
    }