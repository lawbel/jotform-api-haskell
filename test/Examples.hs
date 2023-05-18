{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.Aeson.Optics (key, _String)
import Data.ByteString qualified as Byte.Str
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.ByteString.Lazy.Char8 qualified as Byte.Lazy.Char8
import Data.Foldable (for_)
import Data.Text.IO qualified as Text.IO
import Network.JotForm (ApiClient, ApiKey)
import Network.JotForm qualified as JotForm
import Optics.Core ((%), (^?))

getForms :: ApiClient -> IO ()
getForms client = do
    forms :: [Value] <- JotForm.getForms client JotForm.defaultListConfig
    for_ forms $ \form -> do
        let title = form ^? key "title" % _String
        Text.IO.putStrLn $ maybe "null" id title

getLatestSubmissions :: ApiClient -> IO ()
getLatestSubmissions client = do
    submissions :: [Value] <- JotForm.getSubmissions client options
    for_ submissions $ \sub -> do
        Byte.Lazy.Char8.putStrLn $ Json.Pretty.encodePretty sub
  where
    options = JotForm.defaultListConfig
        { JotForm.offset = Just 0
        , JotForm.limit = Just 100
        , JotForm.orderBy = Just JotForm.OrderByCreatedAt
        }

-- | Use the API key in either of:
--
-- * test-api-key-eu.txt
-- * test-api-key.txt
--
-- Depending on the file name, use 'JotForm.defaultApiClientEu'
-- or 'JotForm.defaultApiClient'.
getClient :: IO ApiClient
getClient =
    getClientFrom "test-api-key-eu.txt" JotForm.defaultApiClientEu
        <|> getClientFrom "test-api-key.txt" JotForm.defaultApiClient

getClientFrom :: FilePath -> (ApiKey -> IO ApiClient) -> IO ApiClient
getClientFrom fileName mkClient = do
    apiKey <- Byte.Str.readFile fileName
    mkClient $ Byte.Str.Char8.strip apiKey

main :: IO ()
main = do
    client <- getClient
    getForms client
    getLatestSubmissions client
