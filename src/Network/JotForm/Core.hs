module Network.JotForm.Core
    ( -- * API Client
      ApiClient (..)

      -- ** Defaults
    , defaultApiClient
    , defaultApiClient'

      -- ** EU Defaults
    , defaultApiClientEu
    , defaultApiClientEu'

      -- * Core Functionality
    , fetch
    , fetchJson
    , toRequest

      -- ** Parameters
    , Params (..)
    , defaultParams

      -- * Other Types

      -- ** BaseUrl
    , BaseUrl (..)
    , baseUrlToString

      -- ** OutputType
    , OutputType (..)

      -- ** DebugMode
    , DebugMode (..)

      -- * Exceptions

      -- ** JsonException
    , JsonException (..)

      -- * Constants
    , userAgent

      -- * Headers
    , urlEncode
    , acceptJson
    ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json
import Data.ByteString qualified as Byte.Str
import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Network.HTTP.Client (Manager, Request, Response)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as Client.TLS
import Network.HTTP.Types (Header, Method, Query)
import Network.HTTP.Types.Header qualified as Header
import Network.JotForm.Utils qualified as Utils

newtype JsonException = MkJsonException String
    deriving (Eq, Ord, Show, Read)

instance Exception JsonException

data BaseUrl = DefaultBaseUrl | EuBaseUrl
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data OutputType = JsonOutput
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data DebugMode = DebugOn | DebugOff
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data ApiClient = MkApiClient
    { baseUrl :: BaseUrl
    , apiVersion :: Str.ByteString
    , apiKey :: Str.ByteString
    , outputType :: OutputType
    , debugMode :: DebugMode
    , httpManager :: Manager
    }

data Params = MkParams
    { path :: Str.ByteString
    , query :: Query
    , body :: Str.ByteString
    , headers :: [Header]
    , method :: Method
    }
    deriving (Eq, Ord, Read, Show)

defaultParams :: Str.ByteString -> Method -> Params
defaultParams thisPath thisMethod =
    MkParams
        { path = thisPath
        , query = []
        , body = Byte.Str.empty
        , headers = []
        , method = thisMethod
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
-- To pass a different 'Manager' instead use the 'defaultApiClient'' function.
defaultApiClient :: Str.ByteString -> IO ApiClient
defaultApiClient key = do
    manager <- Client.newManager Client.TLS.tlsManagerSettings
    pure $ defaultApiClient' key manager

-- | Creates an 'ApiClient' using default settings, and the given
-- HTTP 'Manager'.
defaultApiClient' :: Str.ByteString -> Manager -> ApiClient
defaultApiClient' key manager =
    MkApiClient
        { baseUrl = DefaultBaseUrl
        , apiVersion = Utils.ascii "v1"
        , apiKey = key
        , outputType = JsonOutput
        , debugMode = DebugOff
        , httpManager = manager
        }

-- | The same as 'defaultApiClient', but is set to use the EU endpoint - use
-- this if your account is in EU Safe mode.
defaultApiClientEu :: Str.ByteString -> IO ApiClient
defaultApiClientEu key = do
    def <- defaultApiClient key
    pure $ def {baseUrl = EuBaseUrl}

-- | The same as 'defaultApiClient'', but is set to use the EU endpoint - use
-- this if your account is in EU Safe mode.
defaultApiClientEu' :: Str.ByteString -> Manager -> ApiClient
defaultApiClientEu' key manager =
    let def = defaultApiClient' key manager
    in  def {baseUrl = EuBaseUrl}

baseUrlToString :: BaseUrl -> Str.ByteString
baseUrlToString = \case
    DefaultBaseUrl -> Utils.ascii "api.jotform.com"
    EuBaseUrl -> Utils.ascii "eu-api.jotform.com"

fetch :: ApiClient -> Params -> IO (Response Lazy.ByteString)
fetch client params = Client.httpLbs request manager
  where
    request = toRequest client params
    manager = httpManager client

fetchJson :: FromJSON a => ApiClient -> Params -> IO (Response a)
fetchJson client params = do
    response <- Client.httpLbs requestJson (httpManager client)
    case Json.eitherDecode $ Client.responseBody response of
        Left err -> throwIO $ MkJsonException err
        Right json -> pure (json <$ response)
  where
    request = toRequest client params
    requestJson = Utils.updateHeaders (acceptJson :) request

-- | Properly handles the methods needed by the JotForm API, which at time
-- of writing are:
--
-- * 'Method.methodGet' (GET)
-- * 'Method.methodPost' (POST)
-- * 'Method.methodDelete' (DELETE)
-- * 'Method.methodPut' (PUT)
toRequest :: ApiClient -> Params -> Request
toRequest client params =
    defaultSecureRequest
        { Client.host = baseUrlToString $ baseUrl client
        , Client.path = versionPath <> path params <> outputPath
        , Client.method = method params
        , Client.queryString = Utils.renderQuery $ query params
        , Client.requestBody = Client.RequestBodyBS $ body params
        , Client.requestHeaders = headers params <> defHeaders
        }
  where
    versionPath = Byte.Str.Char8.cons '/' $ apiVersion client
    outputPath = case outputType client of JsonOutput -> Byte.Str.empty
    defHeaders =
        [ (Utils.headerName "ApiKey", apiKey client)
        , (Header.hUserAgent, userAgent)
        ]

urlEncode :: Header
urlEncode =
    (Header.hContentType, Utils.ascii "application/x-www-form-urlencoded")

acceptJson :: Header
acceptJson = (Header.hAccept, Utils.ascii "application/json")
