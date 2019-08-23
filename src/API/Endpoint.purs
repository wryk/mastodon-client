module App.API.Endpoint where

import Prelude hiding ((/))

import Data.Maybe (Maybe)

type PaginationLimitRep =
    ( limit :: Maybe Int
    )

type PaginationLimit = { | PaginationLimitRep }

type PaginationTimelineRep =
    ( max_id :: Maybe String
    , since_id :: Maybe String
    , min_id :: Maybe String
    | PaginationLimitRep
    )

type PaginationTimeline = { | PaginationTimelineRep }

type PaginationPublicTimelineRep =
    ( local :: Maybe Boolean
    , only_media :: Maybe Boolean
    | PaginationTimelineRep
    )

type PaginationPublicTimeline = { | PaginationPublicTimelineRep }