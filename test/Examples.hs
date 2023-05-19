{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON, Value, (.=))
import Data.Aeson qualified as Json
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

putJsonPretty :: ToJSON a => a -> IO ()
putJsonPretty = Byte.Lazy.Char8.putStrLn . Json.Pretty.encodePretty

getForms :: ApiClient -> IO ()
getForms client = do
    forms :: [Value] <- JotForm.getForms client JotForm.defaultListOptions
    for_ forms $ \form -> do
        let title = form ^? key "title" % _String
        Text.IO.putStrLn $ maybe "null" id title

getLatestSubmissions :: ApiClient -> IO ()
getLatestSubmissions client = do
    submissions :: [Value] <-
        JotForm.getSubmissions client $
            JotForm.defaultListOptions
                { JotForm.offset = Just 0
                , JotForm.limit = Just 100
                , JotForm.orderBy = Just "created_at"
                }
    for_ submissions putJsonPretty

submissionAndFormFilters :: ApiClient -> IO ()
submissionAndFormFilters client = do
    let submissionFilter =
            Json.object
                [ "created_at:gt" .= ("2000-01-01 00:00:00" :: String)
                ]
    submissions :: [Value] <-
        JotForm.getSubmissions client $
            JotForm.defaultListOptions
                { JotForm.filters = Just submissionFilter
                }
    putJsonPretty submissions

    let formFilter =
            Json.object
                [ "new:gt" .= ("0" :: String)
                , "status" .= ("ENABLED" :: String)
                ]
    forms :: [Value] <-
        JotForm.getForms client $
            JotForm.defaultListOptions {JotForm.filters = Just formFilter}
    putJsonPretty forms

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
