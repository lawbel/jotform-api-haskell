{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Data.Aeson (Value)
import Data.Aeson.Optics (key, _String)
import Data.ByteString qualified as Byte.Str
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.Foldable (for_)
import Data.Text.IO qualified as Text.IO
import Network.JotForm (ApiClient, ApiKey)
import Network.JotForm qualified as JotForm
import Optics.Core ((%), (^?))

getFormsExample :: ApiClient -> IO ()
getFormsExample client = do
    forms :: [Value] <- JotForm.getForms client JotForm.defaultListConfig
    for_ forms $ \form -> do
        let title = form ^? key "title" % _String
        Text.IO.putStrLn $ maybe "null" id title

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
    getFormsExample client
