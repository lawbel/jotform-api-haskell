{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.JotForm.Raw.User
--
-- Summary of functions in this module:
--
-- +---------------------+------------------+------------------+-----+--------+
-- | Endpoint \\ Method  | GET              | POST             | PUT | DELETE |
-- +=====================+==================+==================+=====+========+
-- | \/user              | 'getUser'        | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/usage       | 'getUsage'       | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/forms       | 'getForms'       | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/submissions | 'getSubmissions' | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/subusers    | 'getSubUsers'    | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/folders     | 'getFolders'     | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/reports     | 'getReports'     | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/settings    | 'getSettings'    | 'updateSettings' | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+
-- | \/user\/history     | 'getHistory'     | -                | -   | -      |
-- +---------------------+------------------+------------------+-----+--------+

module Network.JotForm.Raw.User
    ( -- * \/user
      getUser

      -- ** \/usage
    , getUsage

      -- ** \/forms
    , getForms

      -- ** \/submissions
    , getSubmissions

      -- ** \/subusers
    , getSubUsers

      -- ** \/folders
    , getFolders

      -- ** \/reports
    , getReports

      -- ** \/settings
    , getSettings
    , updateSettings

      -- ** \/history
    , getHistory
    )
where

import Data.Aeson (FromJSON)
import Data.Map.Strict qualified as Str (Map)
import Data.Text qualified as Str (Text)
import Data.Text qualified as Text.Str
import Network.HTTP.Client (Response)
import Network.HTTP.Types.Method qualified as Method
import Network.JotForm.Base qualified as Base
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Core qualified as Core
import Network.JotForm.Extra qualified as Extra
import Network.JotForm.Types (HistoryOpts, ListOpts)
import Network.JotForm.Types qualified as Types

-- | Get user account details for a JotForm user. Returns a variety of
-- information, including:
--
-- * user account type
-- * avatar URL
-- * name
-- * email
-- * website URL
-- * account limits
getUser :: FromJSON a => ApiClient -> IO (Response a)
getUser = Extra.basicGet "/user"

-- | Get number of form submissions received this month. Returns:
--
-- * number of submissions
-- * number of SSL form submissions
-- * payment form submissions
-- * upload space used by user
getUsage :: FromJSON a => ApiClient -> IO (Response a)
getUsage = Extra.basicGet "/user/usage"

-- | Get a list of forms for this account. Returns basic details such as title
-- of the form, when it was created, number of new and total submissions.
getForms :: FromJSON a => ApiClient -> ListOpts -> IO (Response a)
getForms client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/forms"
            , Core.query = Types.listOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- | Get a list of submissions for this account. Returns basic details such as
-- title of the form, when it was created, number of new and total submissions.
getSubmissions :: FromJSON a => ApiClient -> ListOpts -> IO (Response a)
getSubmissions client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/submissions"
            , Core.query = Types.listOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }

-- | Get a list of sub users for this account. Returns a list of forms
-- and form folders with access privileges.
getSubUsers :: FromJSON a => ApiClient -> IO (Response a)
getSubUsers = Extra.basicGet "/user/subusers"

-- | Get a list of form folders for this account. Returns the name of the
-- folder and owner of the folder for shared folders.
getFolders :: FromJSON a => ApiClient -> IO (Response a)
getFolders = Extra.basicGet "/user/folders"

-- | List of URLS for reports in this account. Returns reports for all of
-- the forms: Excel, CSV, printable charts, embeddable HTML tables.
getReports :: FromJSON a => ApiClient -> IO (Response a)
getReports = Extra.basicGet "/user/reports"

-- | Get user's settings for this account.
getSettings :: FromJSON a => ApiClient -> IO (Response a)
getSettings = Extra.basicGet "/user/settings"

-- | Update user's settings. Returns changes on user settings. Pass new
-- user settings as a 'Str.Map' from keys to values.
updateSettings
    :: FromJSON a
    => ApiClient
    -> Str.Map Str.Text Str.Text
    -> IO (Response a)
updateSettings client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/settings"
            , Core.query = []
            , Core.body = Base.renderQueryText query
            , Core.headers = [Core.urlEncode]
            , Core.method = Method.methodPost
            }
  where
    query = Types.optionsToQuery $ Types.MkOptions options

-- | Get user activity log. Returns things like forms
-- created\/modified\/deleted, account logins and other operations.
getHistory :: FromJSON a => ApiClient -> HistoryOpts -> IO (Response a)
getHistory client options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = "/user/history"
            , Core.query = Types.historyOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }
