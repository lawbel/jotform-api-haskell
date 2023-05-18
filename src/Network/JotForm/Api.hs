module Network.JotForm.Api
    ( OrderBy (..)
    , orderByToString
    , getUser
    , getUsage
    , getForms
    ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value)
import Data.ByteString qualified as Str (ByteString)
import Network.HTTP.Client (Response)
import Network.HTTP.Types qualified as Client
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Core qualified as Core
import Network.JotForm.Utils qualified as Utils

data OrderBy
    = OrderById
    | OrderByUsername
    | OrderByTitle
    | OrderByStatus
    | OrderByCreatedAt
    | OrderByUpdatedAt
    | OrderByNew
    | OrderByAll
    | OrderBySlug
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

orderByToString :: OrderBy -> Str.ByteString
orderByToString = \case
    OrderById -> Utils.ascii "id"
    OrderByUsername -> Utils.ascii "username"
    OrderByTitle -> Utils.ascii "title"
    OrderByStatus -> Utils.ascii "status"
    OrderByCreatedAt -> Utils.ascii "created_at"
    OrderByUpdatedAt -> Utils.ascii "updated_at"
    OrderByNew -> Utils.ascii "new"
    OrderByAll -> Utils.ascii "count"
    OrderBySlug -> Utils.ascii "slug"

plusEncode :: Bool
plusEncode = True

getUser :: FromJSON a => ApiClient -> IO (Response a)
getUser client =
    Core.fetchJson client (Utils.ascii "/user") [] Client.methodGet

getUsage :: FromJSON a => ApiClient -> IO (Response a)
getUsage client =
    Core.fetchJson client (Utils.ascii "/user/usage") [] Client.methodGet

getForms
    :: FromJSON a
    => ApiClient
    -> Maybe Int
    -> Maybe Int
    -> Maybe Value
    -> Maybe OrderBy
    -> IO (Response a)
getForms client offset limit filters order =
    Core.fetchJson client (Utils.ascii "/user/forms") query Client.methodGet
  where
    keys = Utils.ascii <$> ["offset", "limit", "filter", "orderby"]
    vals =
        [ Utils.showAscii <$> offset
        , Utils.showAscii <$> limit
        , Client.urlEncode plusEncode . Utils.encodeStrict <$> filters
        , orderByToString <$> order
        ]
    query = do
        (key, mVal) <- keys `zip` vals
        case mVal of
            Nothing -> empty
            Just val -> pure (key, Just val)
