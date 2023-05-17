module Network.JotForm.Core
    ( ApiClient (..)
    , BaseUrl (..)
    , OutputType (..)
    , DebugMode (..)
    , Path
    , ApiKey
    , userAgent
    , defaultApiClient
    , defaultApiClient'
    , baseUrlToString
    , fetchUrl
    , toRequest
    ) where

import Data.ByteString qualified as Byte.Str
import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Network.HTTP.Client (Manager, Request, Response)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as Client.TLS
import Network.HTTP.Types (Method, Query)
import Network.HTTP.Types.URI qualified as URI
import Network.JotForm.Utils qualified as Utils

type Path = Str.ByteString

type ApiKey = Str.ByteString

data BaseUrl = DefaultBaseUrl | EuBaseUrl
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data OutputType = JsonOutput | XmlOutput
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data DebugMode = DebugOn | DebugOff
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data ApiClient = MkApiClient
    { baseUrl :: BaseUrl
    , apiVersion :: Str.ByteString
    , apiKey :: ApiKey
    , outputType :: OutputType
    , debugMode :: DebugMode
    , httpManager :: Manager
    }

-- | The User-Agent which is used for all the requests made to JotForm.
userAgent :: Str.ByteString
userAgent = Utils.ascii "JOTFORM_HASKELL_WRAPPER"

defaultSecureRequest :: Request
defaultSecureRequest =
    Client.defaultRequest
        { Client.secure = True
        , Client.port = 443
        }

-- | Creates an 'ApiClient' using default settings, and the default
-- TLS-enabled manager config from @http-client-tls@.
--
-- To use a different 'Manager' instead use the 'defaultApiClient'' function.
defaultApiClient :: ApiKey -> IO ApiClient
defaultApiClient key = do
    manager <- Client.newManager Client.TLS.tlsManagerSettings
    pure $ defaultApiClient' key manager

-- | Creates an 'ApiClient' using default settings, and the given
-- HTTP 'Manager'.
defaultApiClient' :: ApiKey -> Manager -> ApiClient
defaultApiClient' key manager =
    MkApiClient
        { baseUrl = DefaultBaseUrl
        , apiVersion = Utils.ascii "v1"
        , apiKey = key
        , outputType = JsonOutput
        , debugMode = DebugOff
        , httpManager = manager
        }

baseUrlToString :: BaseUrl -> Str.ByteString
baseUrlToString = \case
    DefaultBaseUrl -> Utils.ascii "api.jotform.com"
    EuBaseUrl -> Utils.ascii "eu-api.jotform.com"

fetchUrl
    :: ApiClient
    -> Path
    -> Query
    -> Method
    -> IO (Response Lazy.ByteString)
fetchUrl client path query method =
    Client.httpLbs (toRequest client path query method) (httpManager client)

toRequest :: ApiClient -> Path -> Query -> Method -> Request
toRequest client path query method =
    defaultSecureRequest
        { Client.host = baseUrlToString $ baseUrl client
        , Client.path = versionPath <> path <> outputPath
        , Client.method = method
        , Client.queryString = URI.renderQuery False query
        , Client.requestHeaders =
            [ Utils.mkHeader "ApiKey" $ apiKey client
            , Utils.mkHeader "User-Agent" userAgent
            ]
        }
  where
    versionPath = Byte.Str.Char8.cons '/' $ apiVersion client
    outputPath = case outputType client of
        JsonOutput -> Byte.Str.empty
        XmlOutput -> Utils.ascii ".xml"
