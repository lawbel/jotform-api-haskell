module Network.JotForm.Api
    ( OrderBy (..)
    , ListConfig (..)
    , defaultListConfig
    , listConfigToQuery
    , orderByToString
    , getUser
    , getUser'
    , getUsage
    , getUsage'
    , getForms
    , getForms'
    , getSubmissions
    , getSubmissions'
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
import Network.HTTP.Types (Query)
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

data ListConfig = MkListConfig
    { offset :: Maybe Int
    , limit :: Maybe Int
    , filters :: Maybe Value
    , orderBy :: Maybe OrderBy
    }
    deriving (Eq, Ord, Show, Read)

defaultListConfig :: ListConfig
defaultListConfig =
    MkListConfig
        { offset = Nothing
        , limit = Nothing
        , filters = Nothing
        , orderBy = Nothing
        }

listConfigToQuery :: ListConfig -> Query
listConfigToQuery config = do
    (key, mVal) <- keys `zip` vals
    case mVal of
        Nothing -> empty
        Just val -> pure (key, Just val)
  where
    keys = Utils.ascii <$> ["offset", "limit", "filter", "orderby"]
    vals =
        [ Utils.showAscii <$> offset config
        , Utils.showAscii <$> limit config
        , URI.urlEncode plusEncode . Utils.encodeStrict <$> filters config
        , orderByToString <$> orderBy config
        ]

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
        Json.Object obj -> Right obj
        _ -> Left "response is not an object"
    content <- case Json.Map.lookup (Json.Key.fromString "content") object of
        Just con -> Right con
        Nothing -> Left "response has no 'content' field"
    case Json.fromJSON content of
        Json.Success val -> Right val
        Json.Error err -> Left err

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

getForms :: FromJSON a => ApiClient -> ListConfig -> IO a
getForms client config = getForms' client config >>= simplifyIO

getForms' :: FromJSON a => ApiClient -> ListConfig -> IO (Response a)
getForms' client config =
    Core.fetchJson
        client
        (Utils.ascii "/user/forms")
        (listConfigToQuery config)
        Method.methodGet

getSubmissions :: FromJSON a => ApiClient -> ListConfig -> IO a
getSubmissions client config = getSubmissions' client config >>= simplifyIO

getSubmissions' :: FromJSON a => ApiClient -> ListConfig -> IO (Response a)
getSubmissions' client config =
    Core.fetchJson
        client
        (Utils.ascii "/user/submissions")
        (listConfigToQuery config)
        Method.methodGet
