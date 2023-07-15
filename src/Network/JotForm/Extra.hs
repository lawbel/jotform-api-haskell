{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.JotForm.Extra

module Network.JotForm.Extra
    ( -- * Response
      simplify
    , simplifyIO

      -- * Map
    , mapByKey

      -- * Requests
    , basicRequest
    , basicGet
    , basicDelete
    )
where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, Key, Value)
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json.Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Map.Strict qualified as Map.Str
import Data.Map.Strict qualified as Str (Map)
import Data.Text qualified as Str (Text)
import Data.Traversable (for)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Method qualified as Method
import Network.JotForm.Base qualified as Base
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Core qualified as Core
import Network.JotForm.Types (ID)

-- | Pull out the @\"content\"@ field from a response body and return it.
simplify :: FromJSON a => Response Value -> Either Str.Text a
simplify response = do
    object <- case Client.responseBody response of
        Json.Object obj -> Right obj
        _ -> Left "response is not an object"
    content <- case Json.Map.lookup (Json.Key.fromString "content") object of
        Just con -> Right con
        Nothing -> Left "response has no 'content' field"
    Base.resultToEither $ Json.fromJSON content

-- | Run @simplify@ and throw an exception if it failed.
simplifyIO :: FromJSON a => Response Value -> IO a
simplifyIO = either (throwIO . Core.MkJsonException) pure . simplify

-- helpers

mapByKey
    :: FromJSON a => Key -> [KeyMap Value] -> IO (Str.Map (ID tag) a)
mapByKey key objects = do
    assocs <- for objects $ \object ->
        case toIdValue object of
            Left err -> throwIO $ Core.MkJsonException err
            Right idValue -> pure idValue
    pure $ Map.Str.fromList assocs
  where
    toIdValue object = do
        let mJson = Json.Map.lookup key object
        json <- Base.maybeToEither errMsg mJson
        objId <- Base.resultToEither $ Json.fromJSON json
        value <- Base.resultToEither $ Json.fromJSON $ Json.Object object
        pure (objId, value)
    errMsg = "couldn't find key '" <> Json.Key.toText key <> "'"

basicRequest
    :: FromJSON a => Str.Text -> Method -> ApiClient -> IO (Response a)
basicRequest path method client =
    Core.fetchJson client $ Core.defParams path method

basicGet :: FromJSON a => Str.Text -> ApiClient -> IO (Response a)
basicGet path = basicRequest path Method.methodGet

basicDelete :: FromJSON a => Str.Text -> ApiClient -> IO (Response a)
basicDelete path = basicRequest path Method.methodDelete
