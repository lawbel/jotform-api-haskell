{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.JotForm.Api.User
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
-- |                     +------------------+                  |     |        |
-- |                     | 'getFormsByID'   |                  |     |        |
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

module Network.JotForm.Api.User
    ( -- * \/user
      getUser

      -- ** \/usage
    , getUsage

      -- ** \/forms
    , getForms
    , getFormsByID

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
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Extra qualified as Extra
import Network.JotForm.Raw.User qualified as Raw
import Network.JotForm.Types (Form, HistoryOpts, ID, ListOpts)

-- | Get user account details for a JotForm user. Returns a variety of
-- information, including:
--
-- * user account type
-- * avatar URL
-- * name
-- * email
-- * website URL
-- * account limits
getUser :: FromJSON a => ApiClient -> IO a
getUser client = Raw.getUser client >>= Extra.simplifyIO

-- | Get number of form submissions received this month. Returns:
--
-- * number of submissions
-- * number of SSL form submissions
-- * payment form submissions
-- * upload space used by user
getUsage :: FromJSON a => ApiClient -> IO a
getUsage client = Raw.getUsage client >>= Extra.simplifyIO

-- | Get a list of forms for this account. Returns basic details such as title
-- of the form, when it was created, number of new and total submissions.
getForms :: FromJSON a => ApiClient -> ListOpts -> IO a
getForms client options = Raw.getForms client options >>= Extra.simplifyIO

-- | An alternate version of 'getForms' which handles extracting the
-- @'ID' 'Form'@ values from the result and strongly-typing them.
getFormsByID
    :: FromJSON a
    => ApiClient
    -> ListOpts
    -> IO (Str.Map (ID Form) a)
getFormsByID client options = getForms client options >>= Extra.mapByKey "id"

-- | Get a list of submissions for this account. Returns basic details such as
-- title of the form, when it was created, number of new and total submissions.
getSubmissions :: FromJSON a => ApiClient -> ListOpts -> IO a
getSubmissions client options =
    Raw.getSubmissions client options >>= Extra.simplifyIO

-- | Get a list of sub users for this account. Returns a list of forms
-- and form folders with access privileges.
getSubUsers :: FromJSON a => ApiClient -> IO a
getSubUsers client = Raw.getSubUsers client >>= Extra.simplifyIO

-- | Get a list of form folders for this account. Returns the name of the
-- folder and owner of the folder for shared folders.
getFolders :: FromJSON a => ApiClient -> IO a
getFolders client = Raw.getFolders client >>= Extra.simplifyIO

-- | List of URLS for reports in this account. Returns reports for all of
-- the forms: Excel, CSV, printable charts, embeddable HTML tables.
getReports :: FromJSON a => ApiClient -> IO a
getReports client = Raw.getReports client >>= Extra.simplifyIO

-- | Get user's settings for this account.
getSettings :: FromJSON a => ApiClient -> IO a
getSettings client = Raw.getSettings client >>= Extra.simplifyIO

-- | Update user's settings. Returns changes on user settings. Pass new
-- user settings as a 'Str.Map' from keys to values.
updateSettings
    :: FromJSON a
    => ApiClient
    -> Str.Map Str.Text Str.Text
    -> IO a
updateSettings client options =
    Raw.updateSettings client options >>= Extra.simplifyIO

-- | Get user activity log. Returns things like forms
-- created\/modified\/deleted, account logins and other operations.
getHistory :: FromJSON a => ApiClient -> HistoryOpts -> IO a
getHistory client options = Raw.getHistory client options >>= Extra.simplifyIO
