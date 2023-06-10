{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON, Value, (.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.Aeson.Optics (key, _Integer, _String)
import Data.ByteString qualified as Byte.Str
import Data.ByteString.Lazy.Char8 qualified as Byte.Lazy.Char8
import Data.Foldable (for_)
import Data.Text qualified as Str (Text)
import Data.Text qualified as Text.Str
import Data.Text.Encoding qualified as Text.Str.Enc
import Network.JotForm (ApiClient)
import Network.JotForm qualified as JotForm
import Optics.Core ((%), (^?))
import System.Exit (exitSuccess)
import Text.Printf (printf)

putJsonPretty :: ToJSON a => a -> IO ()
putJsonPretty = Byte.Lazy.Char8.putStrLn . Json.Pretty.encodePretty

getForms :: ApiClient -> IO ()
getForms client = do
    forms :: [Value] <- JotForm.getForms client JotForm.defListOpts
    for_ forms $ \form -> do
        let title = (form ^? key "title" % _String) =? "-"
        let total = (form ^? key "count" % _String % _Integer) =? 0
        let new = (form ^? key "new" % _String % _Integer) =? 0
        printf "%s (total: %d, new: %d)\n" title total new
  where
    optional =? def = maybe def id optional

getLatestSubmissions :: ApiClient -> IO ()
getLatestSubmissions client = do
    submissions :: [Value] <-
        JotForm.getSubmissions client $
            JotForm.defListOpts
                { JotForm.offset = Just 0
                , JotForm.limit = Just 100
                , JotForm.orderBy = Just "created_at"
                }
    for_ submissions putJsonPretty

submissionAndFormFilters :: ApiClient -> IO ()
submissionAndFormFilters client = do
    let submissionFilter =
            Json.object
                [ "created_at:gt" .= ("2020-02-20" :: String)
                ]
    submissions :: [Value] <-
        JotForm.getSubmissions client $
            JotForm.defListOpts
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
            JotForm.defListOpts {JotForm.filters = Just formFilter}
    putJsonPretty forms

-- | Use the API key in either of:
--
-- * test-api-key-eu.txt
-- * test-api-key.txt
--
-- Depending on the file name, use 'JotForm.defaultApiClientEu'
-- or 'JotForm.defaultApiClient'.
--
-- If neither file exists, simply quit without returning a failure exit code.
getClient :: IO ApiClient
getClient =
    getClientFrom "test-api-key-eu.txt" JotForm.defApiClientEu
        <|> getClientFrom "test-api-key.txt" JotForm.defApiClient
        <|> exit
  where
    exit = do
        putStrLn "no file 'test-api-key[-eu].txt'"
        exitSuccess

getClientFrom :: FilePath -> (Str.Text -> IO ApiClient) -> IO ApiClient
getClientFrom fileName mkClient = do
    apiKey <- Text.Str.Enc.decodeUtf8 <$> Byte.Str.readFile fileName
    mkClient $ Text.Str.strip apiKey

main :: IO ()
main = do
    client <- getClient
    for_ examples $ \ex -> ex client
  where
    examples =
        [ getForms
        , getLatestSubmissions
        , submissionAndFormFilters
        ]
