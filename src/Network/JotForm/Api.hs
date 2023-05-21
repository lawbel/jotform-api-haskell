{-# LANGUAGE DerivingVia #-}

module Network.JotForm.Api
    ( -- * Functions Provided
      -- $intro

      -- * Table Overview
      -- $summary

      -- * API Endpoints

      -- ** \/user
      getUser
    , getUser'

      -- *** \/user\/usage
    , getUsage
    , getUsage'

      -- *** \/user\/forms
    , getForms
    , getForms'

      -- *** \/user\/submissions
    , getSubmissions
    , getSubmissions'

      -- *** \/user\/subusers
    , getSubUsers
    , getSubUsers'

      -- *** \/user\/folders
    , getFolders
    , getFolders'

      -- *** \/user\/reports
    , getReports
    , getReports'

      -- *** \/user\/settings
    , getSettings
    , getSettings'
    , updateSettings
    , updateSettings'

      -- *** \/user\/history
    , getHistory
    , getHistory'

      -- ** \/form

      -- *** \/form\/{id}
    , getForm
    , getForm'

      -- *** \/form\/{id}\/questions
    , getFormQuestions
    , getFormQuestions'

      -- *** \/form\/{id}\/question\/{qid}
    , getFormQuestion
    , getFormQuestion'

      -- * Helper Types

      -- ** ListOptions
    , ListOptions (..)
    , defaultListOptions
    , listOptionsToQuery

      -- ** HistoryOptions
    , HistoryOptions (..)
    , defaultHistoryOptions
    , historyOptionsToQuery

      -- ** Options
    , Options (..)
    , optionsToQuery

      -- ** FormId
    , FormId (..)

      -- ** QuestionId
    , QuestionId (..)
    ) where

import Control.Applicative (empty)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, Value)
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json.Key
import Data.Aeson.KeyMap qualified as Json.Map
import Data.ByteString qualified as Str (ByteString)
import Data.HashMap.Strict qualified as HashMap.Str
import Data.HashMap.Strict qualified as Str (HashMap)
import Data.String (IsString)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (Query)
import Network.HTTP.Types.Method qualified as Method
import Network.HTTP.Types.URI qualified as URI
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Core qualified as Core
import Network.JotForm.Utils qualified as Utils

-- $intro
--
-- For every function available which interacts with a part of the JotForm
-- API, we provide two variants: @function@ and @function'@.
--
-- The first variant, @function@, will be simpler to use as it just returns
-- the content of the JSON body of the response. For example, a call
-- to 'getUser' might return (after tidying up the output):
--
-- > {
-- >     "name": "Example User",
-- >     "email": "example@user.com",
-- >     ...
-- >     "status": "ACTIVE"
-- > }
--
-- The second variant, @function'@, returns the raw 'Response' in full, so
-- you can inspect e.g. the response headers if so needed. So a call to
-- 'getUser'' might instead return (after tidying up the output):
--
-- > Response
-- >     { responseStatus = Status
-- >         { statusCode = 200
-- >         , statusMessage = "OK" }
-- >     , responseHeaders =
-- >         [ ("Content-Type", "application/json")
-- >         , ... ]
-- >     , responseBody =
-- >         {
-- >             "content": {
-- >                 "name": "Example User",
-- >                 "email": "example@user.com",
-- >                 ...
-- >                 "status": "ACTIVE"
-- >             },
-- >             "duration": "12.34ms",
-- >             "limit-left": 567,
-- >             ...
-- >         }
-- >     , responseVersion = HTTP/1.1
-- >     , ... }

-- $summary
--
-- Below is a table summary of the API functions available.
--
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | Endpoint \\ Method              | GET                | POST             | PUT | DELETE |
-- +=================================+====================+==================+=====+========+
-- | @\/user@                        | 'getUser'          | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/usage@                 | 'getUsage'         | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/forms@                 | 'getForms'         | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/submissions@           | 'getSubmissions'   | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/subusers@              | 'getSubUsers'      | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/folders@               | 'getFolders'       | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/reports@               | 'getReports'       | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/settings@              | 'getSettings'      | 'updateSettings' | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/user\/history@               | 'getHistory'       | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/form\/{id}@                  | 'getForm'          | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/form\/{id}\/questions@       | 'getFormQuestions' | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+
-- | @\/form\/{id}\/question\/{qid}@ | 'getFormQuestion'  | -                | -   | -      |
-- +---------------------------------+--------------------+------------------+-----+--------+

-- | A collection of key-value options.
newtype Options = MkOptions
    { unOptions :: Str.HashMap Str.ByteString Str.ByteString
    }
    deriving (Eq, Ord, Show, Read)

newtype FormId = MkFormId {unFormId :: String}
    deriving (Eq, Ord, Show, Read)
    deriving (IsString, Semigroup, Monoid) via String

newtype QuestionId = MkQuestionId {unQuestionId :: String}
    deriving (Eq, Ord, Show, Read)
    deriving (IsString, Semigroup, Monoid) via String

-- | A bundle of options that are re-used in a couple of places in the API
-- where it can potentially return a (very) long list of values.
data ListOptions = MkListOptions
    { offset :: Maybe Int
    , limit :: Maybe Int
    , filters :: Maybe Value
    , orderBy :: Maybe Str.ByteString
    }
    deriving (Eq, Ord, Show, Read)

data HistoryOptions = MkHistoryOptions
    { action :: Maybe Str.ByteString
    , date :: Maybe Str.ByteString
    , sortBy :: Maybe Str.ByteString
    , startDate :: Maybe Str.ByteString
    , endDate :: Maybe Str.ByteString
    }
    deriving (Eq, Ord, Show, Read)

-- | A default 'ListOptions' value; it simply sets 'Nothing' as the value
-- of each option, so that none of these options is specified.
defaultListOptions :: ListOptions
defaultListOptions =
    MkListOptions
        { offset = Nothing
        , limit = Nothing
        , filters = Nothing
        , orderBy = Nothing
        }

-- | Conversion function from 'ListOptions' to 'Query' - not something that
-- end users will normally need, but provided just in case.
listOptionsToQuery :: ListOptions -> Query
listOptionsToQuery options = do
    (key, mVal) <- keys `zip` vals
    case mVal of
        Nothing -> empty
        Just val -> pure (key, Just val)
  where
    keys = Utils.ascii <$> ["offset", "limit", "filter", "orderby"]
    vals =
        [ Utils.showAscii <$> offset options
        , Utils.showAscii <$> limit options
        , Utils.encodeStrict <$> filters options
        , orderBy options
        ]

-- | Conversion function from 'Options' to 'Query' - not something that
-- end users will normally need, but provided just in case.
optionsToQuery :: Options -> Query
optionsToQuery = URI.simpleQueryToQuery . HashMap.Str.toList . unOptions

historyOptionsToQuery :: HistoryOptions -> Query
historyOptionsToQuery options = do
    (key, mVal) <- keys `zip` vals
    case mVal of
        Nothing -> empty
        Just val -> pure (key, Just val)
  where
    keys = Utils.ascii <$> ["action", "date", "sortBy", "startDate", "endDate"]
    vals =
        [ action options
        , date options
        , sortBy options
        , startDate options
        , endDate options
        ]

-- | A default 'HistoryOptions' value; it simply sets 'Nothing' as the
-- value of each option, so that none of these options is specified.
defaultHistoryOptions :: HistoryOptions
defaultHistoryOptions =
    MkHistoryOptions
        { action = Nothing
        , date = Nothing
        , sortBy = Nothing
        , startDate = Nothing
        , endDate = Nothing
        }

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

-- | Run @simplify@ and throw an exception if it failed.
simplifyIO :: FromJSON a => Response Value -> IO a
simplifyIO = either (throwIO . Core.MkJsonException) pure . simplify

-- /user

getUser :: FromJSON a => ApiClient -> IO a
getUser client = getUser' client >>= simplifyIO

getUser' :: FromJSON a => ApiClient -> IO (Response a)
getUser' client =
    Core.fetchJson client (Utils.ascii "/user") [] Method.methodGet

-- /user/usage

getUsage :: FromJSON a => ApiClient -> IO a
getUsage client = getUsage' client >>= simplifyIO

getUsage' :: FromJSON a => ApiClient -> IO (Response a)
getUsage' client =
    Core.fetchJson client (Utils.ascii "/user/usage") [] Method.methodGet

-- /user/forms

getForms :: FromJSON a => ApiClient -> ListOptions -> IO a
getForms client options = getForms' client options >>= simplifyIO

getForms' :: FromJSON a => ApiClient -> ListOptions -> IO (Response a)
getForms' client options =
    Core.fetchJson
        client
        (Utils.ascii "/user/forms")
        (listOptionsToQuery options)
        Method.methodGet

-- /user/submissions

getSubmissions :: FromJSON a => ApiClient -> ListOptions -> IO a
getSubmissions client options = getSubmissions' client options >>= simplifyIO

getSubmissions' :: FromJSON a => ApiClient -> ListOptions -> IO (Response a)
getSubmissions' client options =
    Core.fetchJson
        client
        (Utils.ascii "/user/submissions")
        (listOptionsToQuery options)
        Method.methodGet

-- /user/subusers

getSubUsers :: FromJSON a => ApiClient -> IO a
getSubUsers client = getSubUsers' client >>= simplifyIO

getSubUsers' :: FromJSON a => ApiClient -> IO (Response a)
getSubUsers' client =
    Core.fetchJson client (Utils.ascii "/user/subusers") [] Method.methodGet

-- /user/folders

getFolders :: FromJSON a => ApiClient -> IO a
getFolders client = getFolders' client >>= simplifyIO

getFolders' :: FromJSON a => ApiClient -> IO (Response a)
getFolders' client =
    Core.fetchJson client (Utils.ascii "/user/folders") [] Method.methodGet

-- /user/reports

getReports :: FromJSON a => ApiClient -> IO a
getReports client = getReports' client >>= simplifyIO

getReports' :: FromJSON a => ApiClient -> IO (Response a)
getReports' client =
    Core.fetchJson client (Utils.ascii "/user/reports") [] Method.methodGet

-- /user/settings

getSettings :: FromJSON a => ApiClient -> IO a
getSettings client = getSettings' client >>= simplifyIO

getSettings' :: FromJSON a => ApiClient -> IO (Response a)
getSettings' client =
    Core.fetchJson client (Utils.ascii "/user/settings") [] Method.methodGet

updateSettings :: FromJSON a => ApiClient -> Options -> IO a
updateSettings client options =
    updateSettings' client options >>= simplifyIO

updateSettings' :: FromJSON a => ApiClient -> Options -> IO (Response a)
updateSettings' client options =
    Core.fetchJson
        client
        (Utils.ascii "/user/settings")
        (optionsToQuery options)
        Method.methodPost

-- /user/history

getHistory :: FromJSON a => ApiClient -> HistoryOptions -> IO a
getHistory client options = getHistory' client options >>= simplifyIO

getHistory' :: FromJSON a => ApiClient -> HistoryOptions -> IO (Response a)
getHistory' client options =
    Core.fetchJson
        client
        (Utils.ascii "/user/history")
        (historyOptionsToQuery options)
        Method.methodGet

-- /form/{id}

getForm :: FromJSON a => ApiClient -> String -> IO a
getForm client formId = getForm' client formId >>= simplifyIO

getForm' :: FromJSON a => ApiClient -> String -> IO (Response a)
getForm' client formId =
    Core.fetchJson
        client
        (Utils.ascii $ "/form/" <> formId)
        []
        Method.methodGet

-- /form/{id}/questions

getFormQuestions :: FromJSON a => ApiClient -> FormId -> IO a
getFormQuestions client formId = getFormQuestions' client formId >>= simplifyIO

getFormQuestions' :: FromJSON a => ApiClient -> FormId -> IO (Response a)
getFormQuestions' client formId =
    Core.fetchJson client (Utils.ascii path) [] Method.methodGet
  where
    path = "/form/" <> unFormId formId <> "/questions"

-- /form/{id}/question/{qid}

getFormQuestion
    :: FromJSON a => ApiClient -> FormId -> QuestionId -> IO a
getFormQuestion client formId qId =
    getFormQuestion' client formId qId >>= simplifyIO

getFormQuestion'
    :: FromJSON a => ApiClient -> FormId -> QuestionId -> IO (Response a)
getFormQuestion' client formId qId =
    Core.fetchJson client (Utils.ascii path) [] Method.methodGet
  where
    path = "/form/" <> unFormId formId <> "/question/" <> unQuestionId qId
