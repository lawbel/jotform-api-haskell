{-# LANGUAGE DerivingVia #-}

module Network.JotForm.Api
    ( -- * Functions Provided
      -- $intro

      -- ** Table Overview
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

      -- *** \/form\/{id}\/submissions
    , getFormSubmissions
    , getFormSubmissions'
    , createFormSubmission
    , createFormSubmission'
    , createFormSubmissions
    , createFormSubmissions'

      -- *** \/form\/{id}\/files
    , getFormFiles
    , getFormFiles'

      -- *** \/form\/{id}\/webhooks
    , getFormWebhooks
    , getFormWebhooks'

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

      -- ** ID Type
    , ID (..)

      -- *** Tags
    , Form
    , Question
    ) where

import Control.Applicative (empty)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, Value)
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json.Key
import Data.Aeson.KeyMap qualified as Json.Map
import Data.ByteString qualified as Byte.Str
import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Char8 qualified as Byte.Str.Char8
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
-- Below is a table summarizing the API functions available.
--
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | Endpoint \\ Method              | GET                  | POST                   | PUT                     | DELETE |
-- +=================================+======================+========================+=========================+========+
-- | @\/user@                        | 'getUser'            | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/usage@                 | 'getUsage'           | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/forms@                 | 'getForms'           | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/submissions@           | 'getSubmissions'     | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/subusers@              | 'getSubUsers'        | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/folders@               | 'getFolders'         | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/reports@               | 'getReports'         | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/settings@              | 'getSettings'        | 'updateSettings'       | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/user\/history@               | 'getHistory'         | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/form\/{id}@                  | 'getForm'            | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/form\/{id}\/questions@       | 'getFormQuestions'   | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/form\/{id}\/question\/{qid}@ | 'getFormQuestion'    | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/form\/{id}\/submissions@     | 'getFormSubmissions' | 'createFormSubmission' | 'createFormSubmissions' | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/form\/{id}\/files@           | 'getFormFiles'       | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+
-- | @\/form\/{id}\/webhooks@        | 'getFormWebhooks'    | -                      | -                       | -      |
-- +---------------------------------+----------------------+------------------------+-------------------------+--------+

-- | A collection of key-value options.
newtype Options = MkOptions
    { unOptions :: Str.HashMap Str.ByteString Str.ByteString
    }
    deriving (Eq, Ord, Show, Read)

newtype ID ty = MkID {unID :: Str.ByteString}
    deriving (Eq, Ord, Show, Read)
    deriving (IsString, Semigroup, Monoid) via Str.ByteString

data Form
data Question

-- | A bundle of options that are re-used in a couple of places in the API
-- where it can potentially return a (very) long list of values.
--
-- * if @offset = Nothing@ then the default value used by JotForm is 20
-- * if @limit = Nothing@ then the default value used by JotForm is 20
-- * the maximum @limit@ is 1000
data ListOptions = MkListOptions
    { offset :: Maybe Int
    -- ^ start of each result list; useful for pagination
    , limit :: Maybe Int
    -- ^ number of results in each result list
    , filters :: Maybe Value
    -- ^ filters the query results to fetch a specific submissions range
    , orderBy :: Maybe Str.ByteString
    -- ^ order results by a field name
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
    Core.fetchJson client $
        Core.defaultParams (Utils.ascii "/user") Method.methodGet

-- /user/usage

getUsage :: FromJSON a => ApiClient -> IO a
getUsage client = getUsage' client >>= simplifyIO

getUsage' :: FromJSON a => ApiClient -> IO (Response a)
getUsage' client =
    Core.fetchJson client $
        Core.defaultParams (Utils.ascii "/user/usage") Method.methodGet

-- /user/forms

getForms :: FromJSON a => ApiClient -> ListOptions -> IO a
getForms client options = getForms' client options >>= simplifyIO

getForms' :: FromJSON a => ApiClient -> ListOptions -> IO (Response a)
getForms' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = Utils.ascii "/user/forms"
            , Core.query = listOptionsToQuery options
            , Core.body = Byte.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- /user/submissions

getSubmissions :: FromJSON a => ApiClient -> ListOptions -> IO a
getSubmissions client options = getSubmissions' client options >>= simplifyIO

getSubmissions' :: FromJSON a => ApiClient -> ListOptions -> IO (Response a)
getSubmissions' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = Utils.ascii "/user/submissions"
            , Core.query = listOptionsToQuery options
            , Core.body = Byte.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- /user/subusers

getSubUsers :: FromJSON a => ApiClient -> IO a
getSubUsers client = getSubUsers' client >>= simplifyIO

getSubUsers' :: FromJSON a => ApiClient -> IO (Response a)
getSubUsers' client =
    Core.fetchJson client $
        Core.defaultParams (Utils.ascii "/user/subusers") Method.methodGet

-- /user/folders

getFolders :: FromJSON a => ApiClient -> IO a
getFolders client = getFolders' client >>= simplifyIO

getFolders' :: FromJSON a => ApiClient -> IO (Response a)
getFolders' client =
    Core.fetchJson client $
        Core.defaultParams (Utils.ascii "/user/folders") Method.methodGet

-- /user/reports

getReports :: FromJSON a => ApiClient -> IO a
getReports client = getReports' client >>= simplifyIO

getReports' :: FromJSON a => ApiClient -> IO (Response a)
getReports' client =
    Core.fetchJson client $
        Core.defaultParams (Utils.ascii "/user/reports") Method.methodGet

-- /user/settings

getSettings :: FromJSON a => ApiClient -> IO a
getSettings client = getSettings' client >>= simplifyIO

getSettings' :: FromJSON a => ApiClient -> IO (Response a)
getSettings' client =
    Core.fetchJson client $
        Core.defaultParams (Utils.ascii "/user/settings") Method.methodGet

updateSettings :: FromJSON a => ApiClient -> Options -> IO a
updateSettings client options =
    updateSettings' client options >>= simplifyIO

updateSettings' :: FromJSON a => ApiClient -> Options -> IO (Response a)
updateSettings' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = Utils.ascii "/user/settings"
            , Core.query = []
            , Core.body = Utils.renderQuery $ optionsToQuery options
            , Core.headers = [Core.urlEncode]
            , Core.method = Method.methodPost
            }

-- /user/history

getHistory :: FromJSON a => ApiClient -> HistoryOptions -> IO a
getHistory client options = getHistory' client options >>= simplifyIO

getHistory' :: FromJSON a => ApiClient -> HistoryOptions -> IO (Response a)
getHistory' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = Utils.ascii "/user/history"
            , Core.query = historyOptionsToQuery options
            , Core.body = Byte.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- /form/{id}

getForm :: FromJSON a => ApiClient -> ID Form -> IO a
getForm client formID = getForm' client formID >>= simplifyIO

getForm' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getForm' client formID =
    Core.fetchJson client $
        Core.defaultParams path Method.methodGet
  where
    path = Utils.ascii "/form/" <> unID formID

-- /form/{id}/questions

getFormQuestions :: FromJSON a => ApiClient -> ID Form -> IO a
getFormQuestions client formID = getFormQuestions' client formID >>= simplifyIO

getFormQuestions' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormQuestions' client formID =
    Core.fetchJson client $
        Core.defaultParams (mconcat parts) Method.methodGet
  where
    parts =
        [ Utils.ascii "/form/"
        , unID formID
        , Utils.ascii "/questions"
        ]

-- /form/{id}/question/{qid}

getFormQuestion
    :: FromJSON a => ApiClient -> ID Form -> ID Question -> IO a
getFormQuestion client formID qID =
    getFormQuestion' client formID qID >>= simplifyIO

getFormQuestion'
    :: FromJSON a => ApiClient -> ID Form -> ID Question -> IO (Response a)
getFormQuestion' client formID qID =
    Core.fetchJson client $
        Core.defaultParams (mconcat parts) Method.methodGet
  where
    parts =
        [ Utils.ascii "/form/"
        , unID formID
        , Utils.ascii "/question/"
        , unID qID
        ]

-- /form/{id}/submissions

getFormSubmissions
    :: FromJSON a => ApiClient -> ID Form -> ListOptions -> IO a
getFormSubmissions client formID options =
    getFormSubmissions' client formID options >>= simplifyIO

getFormSubmissions'
    :: FromJSON a => ApiClient -> ID Form -> ListOptions -> IO (Response a)
getFormSubmissions' client formID options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = mconcat parts
            , Core.query = listOptionsToQuery options
            , Core.body = Byte.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }
  where
    parts =
        [ Utils.ascii "/form/"
        , unID formID
        , Utils.ascii "/submissions"
        ]

createFormSubmission
    :: FromJSON a => ApiClient -> ID Form -> Options -> IO a
createFormSubmission client formID submission =
    createFormSubmission' client formID submission >>= simplifyIO

createFormSubmission'
    :: FromJSON a => ApiClient -> ID Form -> Options -> IO (Response a)
createFormSubmission' client formID submission =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = mconcat parts
            , Core.query = []
            , Core.body = Utils.renderQuery query
            , Core.headers = [Core.urlEncode]
            , Core.method = Method.methodPost
            }
  where
    query = optionsToQuery $ mapKeys submission
    mapKeys = MkOptions . HashMap.Str.mapKeys questionName . unOptions
    parts =
        [ Utils.ascii "/form/"
        , unID formID
        , Utils.ascii "/submissions"
        ]

questionName :: Str.ByteString -> Str.ByteString
questionName field = case Byte.Str.Char8.elemIndex '_' field of
    Just i ->
        let (left, right) = Byte.Str.Char8.splitAt i field
        in  mconcat
                [ Utils.ascii "submission["
                , left
                , Utils.ascii "]["
                , Byte.Str.Char8.drop 1 right
                , Utils.ascii "]"
                ]
    Nothing -> Utils.ascii "submission[" <> field <> Utils.ascii "]"

createFormSubmissions
    :: FromJSON a => ApiClient -> ID Form -> Value -> IO a
createFormSubmissions client formID submissions =
    createFormSubmissions' client formID submissions >>= simplifyIO

createFormSubmissions'
    :: FromJSON a => ApiClient -> ID Form -> Value -> IO (Response a)
createFormSubmissions' client formID submissions =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = mconcat parts
            , Core.query = []
            , Core.body = Utils.encodeStrict submissions
            , Core.headers = []
            , Core.method = Method.methodPut
            }
  where
    parts =
        [ Utils.ascii "/form/"
        , unID formID
        , Utils.ascii "/submissions"
        ]

-- /form/{id}/files

getFormFiles :: FromJSON a => ApiClient -> ID Form -> IO a
getFormFiles client formID = getFormFiles' client formID >>= simplifyIO

getFormFiles' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormFiles' client formID =
    Core.fetchJson client $
        Core.defaultParams path Method.methodGet
  where
    path = mconcat parts
    parts =
        [ Utils.ascii "/form/"
        , unID formID
        , Utils.ascii "/files"
        ]

-- /form/{id}/webhooks

getFormWebhooks :: FromJSON a => ApiClient -> ID Form -> IO a
getFormWebhooks client formID = getFormWebhooks' client formID >>= simplifyIO

getFormWebhooks' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormWebhooks' client formID =
    Core.fetchJson client $
        Core.defaultParams path Method.methodGet
  where
    path = mconcat parts
    parts =
        [ Utils.ascii "/form/"
        , unID formID
        , Utils.ascii "/webhooks"
        ]
