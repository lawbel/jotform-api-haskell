{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JotForm.Api
    ( -- * Functions Provided #functions#
      -- $intro

      -- ** Table Overview
      -- $summary

      -- * API Endpoints

      -- ** \/user

      -- | [GET \/user](https://api.jotform.com/docs/#user)
      getUser
    , getUser'

      -- *** \/usage

      -- | [GET \/user\/usage](https://api.jotform.com/docs/#user-usage)
    , getUsage
    , getUsage'

      -- *** \/forms

      -- | [GET \/user\/forms](https://api.jotform.com/docs/#user-usage)
    , getForms
    , getForms'

      -- *** \/submissions

      -- | [GET \/user\/submissions](https://api.jotform.com/docs/#user-submissions)
    , getSubmissions
    , getSubmissions'

      -- *** \/subusers

      -- | [GET \/user\/subusers](https://api.jotform.com/docs/#user-subusers)
    , getSubUsers
    , getSubUsers'

      -- *** \/folders

      -- | [GET \/user\/folders](https://api.jotform.com/docs/#user-folders)
    , getFolders
    , getFolders'

      -- *** \/reports

      -- | [GET \/user\/reports](https://api.jotform.com/docs/#user-reports)
    , getReports
    , getReports'

      -- *** \/settings

      -- | [GET \/user\/settings](https://api.jotform.com/docs/#user-settings)
    , getSettings
    , getSettings'
      -- | [POST \/user\/settings](https://api.jotform.com/docs/#post-user-settings)
    , updateSettings
    , updateSettings'

      -- *** \/history

      -- | [GET \/user\/history](https://api.jotform.com/docs/#user-history)
    , getHistory
    , getHistory'

      -- ** \/form

      -- *** \/{id}

      -- | [GET \/form\/{id}](https://api.jotform.com/docs/#form-id)
    , getForm
    , getForm'

      -- **** \/questions

      -- | [GET \/form\/{id}\/questions](https://api.jotform.com/docs/#form-id-questions)
    , getFormQuestions
    , getFormQuestions'

      -- **** \/question\/{qid}

      -- | [GET \/form\/{id}\/question\/{qid}](https://api.jotform.com/docs/#form-id-question-id)
    , getFormQuestion
    , getFormQuestion'

      -- **** \/submissions

      -- | [GET \/form\/{id}\/submissions](https://api.jotform.com/docs/#form-id-submissions)
    , getFormSubmissions
    , getFormSubmissions'
      -- | [POST \/form\/{id}\/submissions](https://api.jotform.com/docs/#post-form-id-submissions)
    , createFormSubmission
    , createFormSubmission'
      -- | [PUT \/form\/{id}\/submissions](https://api.jotform.com/docs/#put-form-id-submissions)
    , createFormSubmissions
    , createFormSubmissions'

      -- **** \/files

      -- | [GET \/form\/{id}\/files](https://api.jotform.com/docs/#form-id-files)
    , getFormFiles
    , getFormFiles'

      -- **** \/webhooks

      -- | [GET \/form\/{id}\/webhooks](https://api.jotform.com/docs/#form-id-webhooks)
    , getFormWebhooks
    , getFormWebhooks'
      -- | [POST \/form\/{id}\/webhooks](https://api.jotform.com/docs/#post-form-id-webhooks)
    , createFormWebhook
    , createFormWebhook'

      -- ***** \/{whid}

      -- | [DELETE \/form\/{id}\/webhooks\/{whid}](https://api.jotform.com/docs/#delete-form-id-webhooks)
    , deleteFormWebhook
    , deleteFormWebhook'

      -- * Helper Types

      -- ** ListOpts
    , ListOpts (..)
    , defListOpts
    , listOptsToQuery

      -- ** HistoryOpts
    , HistoryOpts (..)
    , defHistoryOpts
    , historyOptsToQuery

      -- *** Action
    , UserAction (..)
    , renderUserAction

      -- *** DateFilter
    , DateFilter (..)
    , dateFilterToQuery

      -- *** DateRange
    , DateRange (..)
    , renderDateRange

      -- *** SortBy
    , SortBy (..)
    , renderSortBy

      -- ** Options
    , Options (..)
    , optionsToQuery

      -- ** ID Type
    , ID (..)

      -- *** Tags
      -- $tags
    , Form
    , Question
    , Webhook
    ) where

import Control.Applicative (empty)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json.Key
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as HashMap.Str
import Data.HashMap.Strict qualified as Str (HashMap)
import Data.String (IsString)
import Data.Text qualified as Str (Text)
import Data.Text qualified as Text.Str
import Data.These (These (..))
import Data.Time (Day)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (QueryText)
import Network.HTTP.Types.Method qualified as Method
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Core qualified as Core
import Network.JotForm.Utils ((&=))
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
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | Endpoint \\ Method             | GET                  | POST                   | PUT                     | DELETE              |
-- +================================+======================+========================+=========================+=====================+
-- | \/user                         | 'getUser'            | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/usage                  | 'getUsage'           | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/forms                  | 'getForms'           | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/submissions            | 'getSubmissions'     | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/subusers               | 'getSubUsers'        | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/folders                | 'getFolders'         | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/reports                | 'getReports'         | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/settings               | 'getSettings'        | 'updateSettings'       | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/user\/history                | 'getHistory'         | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}                   | 'getForm'            | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/questions        | 'getFormQuestions'   | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/question\/{qid}  | 'getFormQuestion'    | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/submissions      | 'getFormSubmissions' | 'createFormSubmission' | 'createFormSubmissions' | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/files            | 'getFormFiles'       | -                      | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/webhooks         | 'getFormWebhooks'    | 'createFormWebhook'    | -                       | -                   |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/webhooks\/{whid} | -                    | -                      | -                       | 'deleteFormWebhook' |
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+

-- | A collection of key-value options.
newtype Options = MkOptions
    { unOptions :: Str.HashMap Str.Text Str.Text
    }
    deriving (Eq, Ord, Show, Read)

newtype ID ty = MkID {unID :: Str.Text}
    deriving (Eq, Ord, Show, Read)
    deriving (IsString, Semigroup, Monoid, FromJSON, ToJSON) via Str.Text

-- $tags
--
-- These types have no constructors (like 'Data.Void.Void' from @base@), so no
-- values can be constructed for them. They are used with 'ID' as a tag to
-- indicate what kind of ID is expected for the arguments to various functions.

-- | The \'form ID\' is the numbers you see on a form URL. You can get
-- form IDs when you call 'getForms'.
data Form

data Question
data Webhook

-- | A bundle of options that are re-used in a couple of places in the API
-- where it can potentially return a (very) long list of values.
--
-- * if @offset = Nothing@ then the default value used by JotForm is 20
-- * if @limit = Nothing@ then the default value used by JotForm is 20
-- * the maximum @limit@ is 1000
data ListOpts = MkListOpts
    { offset :: Maybe Int
    -- ^ Start of each result list; useful for pagination.
    , limit :: Maybe Int
    -- ^ Number of results in each result list.
    , filters :: Maybe Value
    -- ^ Filters the query results to fetch a specific submissions range.
    , orderBy :: Maybe Str.Text
    -- ^ Order results by a field name.
    }
    deriving (Eq, Ord, Show, Read)

data UserAction
    = AllActions
    | UserCreation
    | UserLogin
    | FormCreation
    | FormUpdate
    | FormDelete
    | FormPurge
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data DateFilter
    = -- | The empty filter.
      NoFilter
    | -- | Limit results by a given preset 'DateRange'.
      RangeFilter DateRange
    | -- | Represents a filter between the start/end dates given (if any).
      -- Given @'StartEndFilter' these@, the possible cases are:
      --
      -- * if @these == 'This' start@, then limit results to after @start@.
      -- * if @these == 'That' end@, then limit results to before @end@.
      -- * if @these == 'These' start end@, then limit results to after
      --   @start@ and before @end@.
      StartEndFilter (These Day Day)
    deriving (Eq, Ord, Show, Read)

data DateRange
    = LastWeek
    | LastMonth
    | Last3Months
    | Last6Months
    | LastYear
    | AllTime
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data SortBy
    = -- | Sort ascending.
      SortAsc
    | -- | Sort descending.
      SortDesc
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

-- | A bundle of options provided for convenience when requesting a
-- history\/queue.
data HistoryOpts = MkHistoryOpts
    { action :: Maybe UserAction
    -- ^ Filter results by activity performed. Default is 'AllActions'.
    , dateFilter :: DateFilter
    -- ^ Limit results by a date filter.
    , sortBy :: Maybe SortBy
    -- ^ Lists results by ascending and descending order.
    }
    deriving (Eq, Ord, Show, Read)

renderUserAction :: UserAction -> Str.Text
renderUserAction = \case
    AllActions -> "all"
    UserCreation -> "userCreation"
    UserLogin -> "userLogin"
    FormCreation -> "formCreation"
    FormUpdate -> "formUpdate"
    FormDelete -> "formDelete"
    FormPurge -> "formPurge"

renderSortBy :: SortBy -> Str.Text
renderSortBy = \case
    SortAsc -> "ASC"
    SortDesc -> "DESC"

renderDateRange :: DateRange -> Str.Text
renderDateRange = \case
    LastWeek -> "lastWeek"
    LastMonth -> "lastMonth"
    Last3Months -> "last3Months"
    Last6Months -> "last6Months"
    LastYear -> "lastYear"
    AllTime -> "all"

-- | Conversion function from 'DateFilter' to 'QueryText' - not something that
-- end users will normally need, but provided just in case.
dateFilterToQuery :: DateFilter -> QueryText
dateFilterToQuery = \case
    NoFilter -> []
    RangeFilter range -> ["date" &= renderDateRange range]
    StartEndFilter (This start) -> [startQuery start]
    StartEndFilter (That end) -> [endQuery end]
    StartEndFilter (These start end) -> [startQuery start, endQuery end]
  where
    startQuery day = "startDate" &= Utils.renderDateJF day
    endQuery day = "endDate" &= Utils.renderDateJF day

-- | A default 'ListOpts' value; it simply sets 'Nothing' as the value
-- of each option, so that none of these options is specified.
defListOpts :: ListOpts
defListOpts =
    MkListOpts
        { offset = Nothing
        , limit = Nothing
        , filters = Nothing
        , orderBy = Nothing
        }

-- | Conversion function from 'ListOpts' to 'QueryText' - not something that
-- end users will normally need, but provided just in case.
listOptsToQuery :: ListOpts -> QueryText
listOptsToQuery options = do
    (key, mVal) <- keys `zip` vals
    case mVal of
        Nothing -> empty
        Just val -> pure (key &= val)
  where
    keys = ["offset", "limit", "filter", "orderby"]
    vals =
        [ Utils.showText <$> offset options
        , Utils.showText <$> limit options
        , Utils.encodeText <$> filters options
        , orderBy options
        ]

-- | Conversion function from 'Options' to 'QueryText' - not something that
-- end users will normally need, but provided just in case.
optionsToQuery :: Options -> QueryText
optionsToQuery = fmap (second Just) . HashMap.Str.toList . unOptions

historyOptsToQuery :: HistoryOpts -> QueryText
historyOptsToQuery options =
    dateFilterToQuery (dateFilter options) <> otherQueries
  where
    otherQueries = do
        (key, mVal) <- keys `zip` vals
        case mVal of
            Nothing -> empty
            Just val -> pure (key &= val)
    keys = ["action", "sortBy"]
    vals =
        [ renderUserAction <$> action options
        , renderSortBy <$> sortBy options
        ]

-- | A default 'HistoryOpts' value; it simply sets 'Nothing'/'NoFilter' as the
-- value of each option, so that none of the options are specified.
defHistoryOpts :: HistoryOpts
defHistoryOpts =
    MkHistoryOpts
        { action = Nothing
        , dateFilter = NoFilter
        , sortBy = Nothing
        }

-- | Pull out the @\"content\"@ field from a response body and return it.
simplify :: FromJSON a => Response Value -> Either Str.Text a
simplify response = do
    object <- case Client.responseBody response of
        Json.Object obj -> Right obj
        _ -> Left "response is not an object"
    content <- case Json.Map.lookup (Json.Key.fromString "content") object of
        Just con -> Right con
        Nothing -> Left "response has no 'content' field"
    case Json.fromJSON content of
        Json.Success val -> Right val
        Json.Error err -> Left $ Text.Str.pack err

-- | Run @simplify@ and throw an exception if it failed.
simplifyIO :: FromJSON a => Response Value -> IO a
simplifyIO = either (throwIO . Core.MkJsonException) pure . simplify

-- /user

-- | Get user account details for a JotForm user.
--
-- Returns a variety of information, including:
--
-- * user account type
-- * avatar URL
-- * name
-- * email
-- * website URL
-- * account limits
getUser :: FromJSON a => ApiClient -> IO a
getUser client = getUser' client >>= simplifyIO

-- | Non-simplified version of 'getUser' - see note
-- [here]("Network.JotForm.Api#g:functions").
getUser' :: FromJSON a => ApiClient -> IO (Response a)
getUser' client =
    Core.fetchJson client $
        Core.defParams "/user" Method.methodGet

-- /user/usage

-- | Get number of form submissions received this month.
--
-- Returns:
--
-- * number of submissions
-- * number of SSL form submissions
-- * payment form submissions
-- * upload space used by user
getUsage :: FromJSON a => ApiClient -> IO a
getUsage client = getUsage' client >>= simplifyIO

-- | Non-simplified version of 'getUsage' - see note
-- [here]("Network.JotForm.Api#g:functions").
getUsage' :: FromJSON a => ApiClient -> IO (Response a)
getUsage' client =
    Core.fetchJson client $
        Core.defParams "/user/usage" Method.methodGet

-- /user/forms

-- | Get a list of forms for this account.
--
-- Returns: basic details such as title of the form, when it was created,
-- number of new and total submissions.
getForms :: FromJSON a => ApiClient -> ListOpts -> IO a
getForms client options = getForms' client options >>= simplifyIO

-- | Non-simplified version of 'getForms' - see note
-- [here]("Network.JotForm.Api#g:functions").
getForms' :: FromJSON a => ApiClient -> ListOpts -> IO (Response a)
getForms' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/forms"
            , Core.query = listOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- /user/submissions

-- | Get a list of submissions for this account.
--
-- Returns: basic details such as title of the form, when it was created,
-- number of new and total submissions.
getSubmissions :: FromJSON a => ApiClient -> ListOpts -> IO a
getSubmissions client options = getSubmissions' client options >>= simplifyIO

-- | Non-simplified version of 'getSubmissions' - see note
-- [here]("Network.JotForm.Api#g:functions").
getSubmissions' :: FromJSON a => ApiClient -> ListOpts -> IO (Response a)
getSubmissions' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/submissions"
            , Core.query = listOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- /user/subusers

-- | Get a list of sub users for this account.
--
-- Returns: list of forms and form folders with access privileges.
getSubUsers :: FromJSON a => ApiClient -> IO a
getSubUsers client = getSubUsers' client >>= simplifyIO

-- | Non-simplified version of 'getSubUsers' - see note
-- [here]("Network.JotForm.Api#g:functions").
getSubUsers' :: FromJSON a => ApiClient -> IO (Response a)
getSubUsers' client =
    Core.fetchJson client $
        Core.defParams "/user/subusers" Method.methodGet

-- /user/folders

-- | Get a list of form folders for this account.
--
-- Returns: name of the folder and owner of the folder for shared folders.
getFolders :: FromJSON a => ApiClient -> IO a
getFolders client = getFolders' client >>= simplifyIO

-- | Non-simplified version of 'getFolders' - see note
-- [here]("Network.JotForm.Api#g:functions").
getFolders' :: FromJSON a => ApiClient -> IO (Response a)
getFolders' client =
    Core.fetchJson client $
        Core.defParams "/user/folders" Method.methodGet

-- /user/reports

-- | List of URLS for reports in this account.
--
-- Returns: reports for all of the forms. ie. Excel, CSV, printable
-- charts, embeddable HTML tables.
getReports :: FromJSON a => ApiClient -> IO a
getReports client = getReports' client >>= simplifyIO

-- | Non-simplified version of 'getReports' - see note
-- [here]("Network.JotForm.Api#g:functions").
getReports' :: FromJSON a => ApiClient -> IO (Response a)
getReports' client =
    Core.fetchJson client $
        Core.defParams "/user/reports" Method.methodGet

-- /user/settings

-- | Get user's settings for this account.
--
-- Returns: user's time zone and language.
getSettings :: FromJSON a => ApiClient -> IO a
getSettings client = getSettings' client >>= simplifyIO

-- | Non-simplified version of 'getSettings' - see note
-- [here]("Network.JotForm.Api#g:functions").
getSettings' :: FromJSON a => ApiClient -> IO (Response a)
getSettings' client =
    Core.fetchJson client $
        Core.defParams "/user/settings" Method.methodGet

-- | Update user's settings.
--
-- Returns: changes on user settings.
updateSettings
    :: FromJSON a
    => ApiClient
    -> Options
    -- ^ New user settings, specified as keys and values.
    -> IO a
updateSettings client options =
    updateSettings' client options >>= simplifyIO

-- | Non-simplified version of 'updateSettings' - see note
-- [here]("Network.JotForm.Api#g:functions").
updateSettings' :: FromJSON a => ApiClient -> Options -> IO (Response a)
updateSettings' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/settings"
            , Core.query = []
            , Core.body = Utils.renderQueryText $ optionsToQuery options
            , Core.headers = [Core.urlEncode]
            , Core.method = Method.methodPost
            }

-- /user/history

-- | Get user activity log.
--
-- Returns: activity log about things like forms created\/modified\/deleted,
-- account logins and other operations.
getHistory :: FromJSON a => ApiClient -> HistoryOpts -> IO a
getHistory client options = getHistory' client options >>= simplifyIO

-- | Non-simplified version of 'getHistory' - see note
-- [here]("Network.JotForm.Api#g:functions").
getHistory' :: FromJSON a => ApiClient -> HistoryOpts -> IO (Response a)
getHistory' client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/history"
            , Core.query = historyOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- /form/{id}

-- | Get basic information about a form.
--
-- Returns: form ID, status, update and creation dates, submission count etc.
getForm
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> IO a
getForm client formID = getForm' client formID >>= simplifyIO

-- | Non-simplified version of 'getForm' - see note
-- [here]("Network.JotForm.Api#g:functions").
getForm' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getForm' client (MkID formID) =
    Core.fetchJson client $
        Core.defParams path Method.methodGet
  where
    path = "/form/" <> formID

-- /form/{id}/questions

-- | Get a list of all questions on a form.
--
-- Returns: question properties of a form.
getFormQuestions
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> IO a
getFormQuestions client formID = getFormQuestions' client formID >>= simplifyIO

-- | Non-simplified version of 'getFormQuestions' - see note
-- [here]("Network.JotForm.Api#g:functions").
getFormQuestions' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormQuestions' client (MkID formID) =
    Core.fetchJson client $
        Core.defParams path Method.methodGet
  where
    path = "/form/" <> formID <> "/questions"

-- /form/{id}/question/{qid}

-- | Get details about a question.
--
-- Returns: question properties like required and validation.
getFormQuestion
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> ID Question
    -- ^ Identifier for each question on a form. You can get a list of
    -- question IDs from 'getFormQuestions'.
    -> IO a
getFormQuestion client formID qID =
    getFormQuestion' client formID qID >>= simplifyIO

-- | Non-simplified version of 'getFormQuestion' - see note
-- [here]("Network.JotForm.Api#g:functions").
getFormQuestion'
    :: FromJSON a => ApiClient -> ID Form -> ID Question -> IO (Response a)
getFormQuestion' client (MkID formID) (MkID qID) =
    Core.fetchJson client $
        Core.defParams path Method.methodGet
  where
    path = "/form/" <> formID <> "/question/" <> qID

-- /form/{id}/submissions

-- | List of a form submissions.
--
-- Returns: submissions of a specific form.
getFormSubmissions
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> ListOpts
    -> IO a
getFormSubmissions client formID options =
    getFormSubmissions' client formID options >>= simplifyIO

-- | Non-simplified version of 'getFormSubmissions' - see note
-- [here]("Network.JotForm.Api#g:functions").
getFormSubmissions'
    :: FromJSON a => ApiClient -> ID Form -> ListOpts -> IO (Response a)
getFormSubmissions' client (MkID formID) options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = listOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }
  where
    path = "/form/" <> formID <> "/submissions"

-- | Submit data to this form using the API.
--
-- Returns: posted submission ID and URL.
createFormSubmission
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> Options
    -- ^ Submission data with question IDs.
    -> IO a
createFormSubmission client formID submission =
    createFormSubmission' client formID submission >>= simplifyIO

-- | Non-simplified version of 'createFormSubmission' - see note
-- [here]("Network.JotForm.Api#g:functions").
createFormSubmission'
    :: FromJSON a => ApiClient -> ID Form -> Options -> IO (Response a)
createFormSubmission' client (MkID formID) submission =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = []
            , Core.body = Utils.renderQueryText query
            , Core.headers = [Core.urlEncode]
            , Core.method = Method.methodPost
            }
  where
    query = optionsToQuery $ mapKeys submission
    mapKeys = MkOptions . HashMap.Str.mapKeys questionName . unOptions
    path = "/form/" <> formID <> "/submissions"

questionName :: Str.Text -> Str.Text
questionName field = case Utils.elemIndexText '_' field of
    Just i ->
        let (left, right) = Text.Str.splitAt i field
        in  "submission[" <> left <> "][" <> Text.Str.drop 1 right <> "]"
    Nothing -> "submission[" <> field <> "]"

-- | Submit data to this form using the API.
--
-- Returns: posted submission ID and URL.
createFormSubmissions
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> Value
    -- ^ Submission data with question IDs.
    -> IO a
createFormSubmissions client formID submissions =
    createFormSubmissions' client formID submissions >>= simplifyIO

-- | Non-simplified version of 'createFormSubmissions' - see note
-- [here]("Network.JotForm.Api#g:functions").
createFormSubmissions'
    :: FromJSON a => ApiClient -> ID Form -> Value -> IO (Response a)
createFormSubmissions' client (MkID formID) submissions =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = []
            , Core.body = Utils.encodeText submissions
            , Core.headers = []
            , Core.method = Method.methodPut
            }
  where
    path = "/form/" <> formID <> "/submissions"

-- /form/{id}/files

-- | List of files uploaded on a form.
--
-- Returns: uploaded file information and URLs on a specific form.
getFormFiles
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> IO a
getFormFiles client formID = getFormFiles' client formID >>= simplifyIO

-- | Non-simplified version of 'getFormFiles' - see note
-- [here]("Network.JotForm.Api#g:functions").
getFormFiles' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormFiles' client (MkID formID) =
    Core.fetchJson client $
        Core.defParams path Method.methodGet
  where
    path = "/form/" <> formID <> "/files"

-- /form/{id}/webhooks

-- | Get list of webhooks for a form.
--
-- Returns: list of webhooks for a specific form.
getFormWebhooks
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> IO a
getFormWebhooks client formID = getFormWebhooks' client formID >>= simplifyIO

-- | Non-simplified version of 'getFormWebhooks' - see note
-- [here]("Network.JotForm.Api#g:functions").
getFormWebhooks' :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormWebhooks' client (MkID formID) =
    Core.fetchJson client $
        Core.defParams path Method.methodGet
  where
    path = "/form/" <> formID <> "/webhooks"

-- | Add a new webhook.
--
-- Returns: list of webhooks for a specific form.
createFormWebhook
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> Str.Text
    -- ^ Webhook URL where form data will be posted when form is submitted.
    -> IO a
createFormWebhook client formID url =
    createFormWebhook' client formID url >>= simplifyIO

-- | Non-simplified version of 'createFormWebhook' - see note
-- [here]("Network.JotForm.Api#g:functions").
createFormWebhook'
    :: FromJSON a => ApiClient -> ID Form -> Str.Text -> IO (Response a)
createFormWebhook' client (MkID formID) url =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = []
            , Core.body = Utils.renderQueryText [query]
            , Core.headers = []
            , Core.method = Method.methodPost
            }
  where
    query = ("webhookURL", Just url)
    path = "/form/" <> formID <> "/webhooks"

-- /form/{id}/webhooks/{whid}

-- | Delete a specific webhook of a form.
--
-- Returns: remaining webhook URLs of form.
deleteFormWebhook
    :: FromJSON a
    => ApiClient
    -> ID Form
    -- ^ \'Form ID\' is the numbers you see on a form URL. You can get
    -- form IDs when you call 'getForms'.
    -> ID Webhook
    -- ^ You can get webhook IDs when you call 'getFormWebhooks'
    -> IO a
deleteFormWebhook client formID whID =
    deleteFormWebhook' client formID whID >>= simplifyIO

-- | Non-simplified version of 'deleteFormWebhook' - see note
-- [here]("Network.JotForm.Api#g:functions").
deleteFormWebhook'
    :: FromJSON a => ApiClient -> ID Form -> ID Webhook -> IO (Response a)
deleteFormWebhook' client (MkID formID) (MkID whID) =
    Core.fetchJson client $
        Core.defParams path Method.methodDelete
  where
    path = "/form/" <> formID <> "/webhooks/" <> whID
