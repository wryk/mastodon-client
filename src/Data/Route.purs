module App.Data.Route where

import Prelude hiding ((/))

import App.Data.Account (AccountId(..))
import App.Data.Status (StatusId(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
-- import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
    = Home
    | Account AccountId
    | Status StatusId
    | HomeTimeline
    | ConversationsTimeline
    | PublicTimeline
    | HashtagTimeline String
    | ListTimeline String

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

-- instance showRoute :: Show Route where
--     show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "Account": "account" / accountId segment
    , "Status": "status" / statusId segment
    , "HomeTimeline": "home" / noArgs
    , "ConversationsTimeline": "conversations" / noArgs
    , "PublicTimeline": "public" / noArgs
    , "HashtagTimeline": "hashtag" / segment
    , "ListTimeline": "list" / segment
    }

accountId :: RouteDuplex' String -> RouteDuplex' AccountId
accountId = as (\(AccountId id) -> id) (Right <<< AccountId)

statusId :: RouteDuplex' String -> RouteDuplex' StatusId
statusId = as (\(StatusId id) -> id) (Right <<< StatusId)