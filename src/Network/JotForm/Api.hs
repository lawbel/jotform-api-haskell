module Network.JotForm.Api
    ( OrderBy (..)
    , orderByToString
    , getUser
    , getUser'
    , getUsage
    , getUsage'
    , getForms
    , getForms'
    ) where

import Control.Applicative (empty)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, Value)
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json.Key
import Data.Aeson.KeyMap qualified as Json.Map
import Data.ByteString qualified as Str (ByteString)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Method qualified as Method
import Network.HTTP.Types.URI qualified as URI
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

-- | Pull out the "content" field from a response body and return it.
simplify :: FromJSON a => Response Value -> Either String a
simplify response = do
    object <- case Client.responseBody response of
        Json.Object obj -> pure obj
        _ -> Left "response is not an object"
    content <- case Json.Map.lookup (Json.Key.fromString "content") object of
        Just con -> pure con
        Nothing -> Left "response has no 'content' field"
    case Json.fromJSON content of
        Json.Error err -> Left err
        Json.Success a -> Right a

-- | Run 'simplify' and throw an exception if it failed.
simplifyIO :: FromJSON a => Response Value -> IO a
simplifyIO = either (throwIO . Core.MkJsonException) pure . simplify

getUser :: FromJSON a => ApiClient -> IO a
getUser client = getUser' client >>= simplifyIO

getUser' :: FromJSON a => ApiClient -> IO (Response a)
getUser' client =
    Core.fetchJson client (Utils.ascii "/user") [] Method.methodGet

getUsage :: FromJSON a => ApiClient -> IO a
getUsage client = getUsage' client >>= simplifyIO

getUsage' :: FromJSON a => ApiClient -> IO (Response a)
getUsage' client =
    Core.fetchJson client (Utils.ascii "/user/usage") [] Method.methodGet

getForms
    :: FromJSON a
    => ApiClient
    -> Maybe Int
    -> Maybe Int
    -> Maybe Value
    -> Maybe OrderBy
    -> IO a
getForms client offset limit filters order =
    getForms' client offset limit filters order >>= simplifyIO

getForms'
    :: FromJSON a
    => ApiClient
    -> Maybe Int
    -> Maybe Int
    -> Maybe Value
    -> Maybe OrderBy
    -> IO (Response a)
getForms' client offset limit filters order =
    Core.fetchJson client (Utils.ascii "/user/forms") query Method.methodGet
  where
    keys = Utils.ascii <$> ["offset", "limit", "filter", "orderby"]
    vals =
        [ Utils.showAscii <$> offset
        , Utils.showAscii <$> limit
        , URI.urlEncode plusEncode . Utils.encodeStrict <$> filters
        , orderByToString <$> order
        ]
    query = do
        (key, mVal) <- keys `zip` vals
        case mVal of
            Nothing -> empty
            Just val -> pure (key, Just val)
