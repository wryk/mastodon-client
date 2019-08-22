module App.Data.Route where

import Prelude hiding ((/))

import App.Data.Account (AccountId(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
    = Home
    | Account AccountId
    | HomeTimeline
    | ConversationsTimeline
    | PublicTimeline
    | HashtagTimeline String
    | ListTimeline String

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
    show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "Account": "account" / accountId segment
    , "HomeTimeline": "home" / noArgs
    , "ConversationsTimeline": "conversations" / noArgs
    , "PublicTimeline": "public" / noArgs
    , "HashtagTimeline": "hashtag" / segment
    , "ListTimeline": "list" / segment
    }

printAccountId :: AccountId -> String
printAccountId (AccountId id) = id

parseAccountId :: String -> Either String AccountId
parseAccountId = Right <<< AccountId

accountId :: RouteDuplex' String -> RouteDuplex' AccountId
accountId = as printAccountId parseAccountId