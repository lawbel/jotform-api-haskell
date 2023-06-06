{-# LANGUAGE OverloadedStrings #-}

module Network.JotForm.Core
    ( -- * API Client
      ApiClient (..)

      -- ** Defaults
    , defApiClient
    , defApiClient'

      -- ** EU Defaults
    , defApiClientEu
    , defApiClientEu'

      -- * Core Functionality
    , fetch
    , fetchJson
    , toRequest

      -- ** Parameters
    , Params (..)
    , defParams

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

      -- * Headers
    , userAgent
    , urlEncode
    , acceptJson
    ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Text qualified as Str (Text)
import Data.Text qualified as Text.Str
import Data.Text.Encoding qualified as Text.Str.Enc
import Network.HTTP.Client (Manager, Request, Response)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as Client.TLS
import Network.HTTP.Types (Header, Method, QueryText)
import Network.HTTP.Types.Header qualified as Header
import Network.JotForm.Utils qualified as Utils

newtype JsonException = MkJsonException Str.Text
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
    , apiVersion :: Str.Text
    , apiKey :: Str.Text
    , outputType :: OutputType
    , debugMode :: DebugMode
    , httpManager :: Manager
    }

data Params = MkParams
    { path :: Str.Text
    , query :: QueryText
    , body :: Str.Text
    , headers :: [Header]
    , method :: Method
    }
    deriving (Eq, Ord, Read, Show)

defParams
    :: Str.Text
    -- ^ URL path
    -> Method
    -> Params
defParams thisPath thisMethod =
    MkParams
        { path = thisPath
        , query = []
        , body = Text.Str.empty
        , headers = []
        , method = thisMethod
        }

defSecureRequest :: Request
defSecureRequest =
    Client.defaultRequest
        { Client.secure = True
        , Client.port = 443
        }

-- | Creates an 'ApiClient' using default settings, and the default
-- TLS-enabled manager config from @http-client-tls@.
--
-- To pass a different 'Manager' instead use the 'defApiClient'' function.
defApiClient :: Str.Text -> IO ApiClient
defApiClient key = do
    manager <- Client.newManager Client.TLS.tlsManagerSettings
    pure $ defApiClient' key manager

-- | Creates an 'ApiClient' using default settings, and the given
-- HTTP 'Manager'.
defApiClient' :: Str.Text -> Manager -> ApiClient
defApiClient' key manager =
    MkApiClient
        { baseUrl = DefaultBaseUrl
        , apiVersion = "v1"
        , apiKey = key
        , outputType = JsonOutput
        , debugMode = DebugOff
        , httpManager = manager
        }

-- | The same as 'defApiClient', but is set to use the EU endpoint - use
-- this if your account is in EU Safe mode.
defApiClientEu :: Str.Text -> IO ApiClient
defApiClientEu key = do
    def <- defApiClient key
    pure $ def {baseUrl = EuBaseUrl}

-- | The same as 'defApiClient'', but is set to use the EU endpoint - use
-- this if your account is in EU Safe mode.
defApiClientEu' :: Str.Text -> Manager -> ApiClient
defApiClientEu' key manager =
    let def = defApiClient' key manager
    in  def {baseUrl = EuBaseUrl}

baseUrlToString :: BaseUrl -> Str.Text
baseUrlToString = \case
    DefaultBaseUrl -> "api.jotform.com"
    EuBaseUrl -> "eu-api.jotform.com"

fetch :: ApiClient -> Params -> IO (Response Lazy.ByteString)
fetch client params = Client.httpLbs request manager
  where
    request = toRequest client params
    manager = httpManager client

fetchJson :: FromJSON a => ApiClient -> Params -> IO (Response a)
fetchJson client params = do
    response <- Client.httpLbs requestJson (httpManager client)
    case Json.eitherDecode $ Client.responseBody response of
        Left err -> throwIO $ MkJsonException $ Text.Str.pack err
        Right json -> pure (json <$ response)
  where
    request = toRequest client params
    requestJson = Utils.updateHeaders (acceptJson :) request

toRequest :: ApiClient -> Params -> Request
toRequest client params =
    defSecureRequest
        { Client.host = encode $ baseUrlToString $ baseUrl client
        , Client.path = encode $ versionPath <> path params <> outputPath
        , Client.method = method params
        , Client.queryString = Utils.renderQueryBytes $ query params
        , Client.requestBody = Client.RequestBodyBS $ encode $ body params
        , Client.requestHeaders = headers params <> defHeaders
        }
  where
    encode = Text.Str.Enc.encodeUtf8
    versionPath = Text.Str.cons '/' $ apiVersion client
    outputPath = case outputType client of JsonOutput -> Text.Str.empty
    defHeaders =
        [ (Utils.headerName "ApiKey", encode $ apiKey client)
        , userAgent
        ]

-- | The User-Agent which is used for all the requests made to JotForm.
userAgent :: Header
userAgent = (Header.hUserAgent, "JOTFORM_HASKELL_WRAPPER")

urlEncode :: Header
urlEncode = (Header.hContentType, "application/x-www-form-urlencoded")

acceptJson :: Header
acceptJson = (Header.hAccept, "application/json")
