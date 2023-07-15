{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.JotForm.Api.Form
--
-- Summary of functions in this module:
--
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+
-- | Endpoint \\ Method             | GET                    | POST                   | PUT                     | DELETE              |
-- +================================+========================+========================+=========================+=====================+
-- | \/form\/{id}                   | 'getForm'              | -                      | -                       | -                   |
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/questions        | 'getFormQuestions'     | -                      | -                       | -                   |
-- |                                +------------------------+                        |                         |                     |
-- |                                | 'getFormQuestionsByID' |                        |                         |                     |
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/question\/{qid}  | 'getFormQuestion'      | -                      | -                       | -                   |
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/submissions      | 'getFormSubmissions'   | 'createFormSubmission' | 'createFormSubmissions' | -                   |
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/files            | 'getFormFiles'         | -                      | -                       | -                   |
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/webhooks         | 'getFormWebhooks'      | 'createFormWebhook'    | -                       | -                   |
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+
-- | \/form\/{id}\/webhooks\/{whid} | -                      | -                      | -                       | 'deleteFormWebhook' |
-- +--------------------------------+------------------------+------------------------+-------------------------+---------------------+

module Network.JotForm.Api.Form
    ( -- * \/form

      -- ** \/{id}
      getForm

      -- *** \/questions
    , getFormQuestions
    , getFormQuestionsByID

      -- *** \/question\/{qid}
    , getFormQuestion

      -- *** \/submissions
    , getFormSubmissions
    , createFormSubmission
    , createFormSubmissions

      -- *** \/files
    , getFormFiles

      -- *** \/webhooks
    , getFormWebhooks
    , createFormWebhook

      -- **** \/{whid}
    , deleteFormWebhook
    )
where

import Data.Aeson (FromJSON, Value)
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Map.Strict qualified as Str (Map)
import Data.Text qualified as Str (Text)
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Extra qualified as Extra
import Network.JotForm.Raw.Form qualified as Raw
import Network.JotForm.Types (Form, ID, ListOpts, Question, Webhook)

-- | Get basic information about a form - form ID, status, update and
-- creation dates, submission count etc.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
getForm :: FromJSON a => ApiClient -> ID Form -> IO a
getForm client formID = Raw.getForm client formID >>= Extra.simplifyIO

-- | Get a list of all questions on a form.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
getFormQuestions :: FromJSON a => ApiClient -> ID Form -> IO a
getFormQuestions client formID =
    Raw.getFormQuestions client formID >>= Extra.simplifyIO

-- | An alternate version of 'getFormQuestions' which handles extracting the
-- @'ID' 'Question'@ values from the result and strongly-typing them.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
getFormQuestionsByID
    :: FromJSON a
    => ApiClient
    -> ID Form
    -> IO (Str.Map (ID Question) a)
getFormQuestionsByID client formID = do
    questions <- getFormQuestions client formID
    Extra.mapByKey "qid" $ Json.Map.elems questions

-- | Returns details about a question like whether it is required and if
-- there is any validation.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
--
-- Similarly, the 'ID' of a 'Question' is an identifier for each question
-- on a form. You can get a list of question IDs from
-- 'Network.JotForm.Api.getFormQuestionsByID'
-- or 'Network.JotForm.Api.getFormQuestions'.
getFormQuestion :: FromJSON a => ApiClient -> ID Form -> ID Question -> IO a
getFormQuestion client formID qID =
    Raw.getFormQuestion client formID qID >>= Extra.simplifyIO

-- | Returns submissions of a specific form.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
getFormSubmissions :: FromJSON a => ApiClient -> ID Form -> ListOpts -> IO a
getFormSubmissions client formID options =
    Raw.getFormSubmissions client formID options >>= Extra.simplifyIO

-- | Submit data to this form using the API. Returns posted submission ID
-- and URL. Pass submission data as a Map from with question IDs.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
createFormSubmission
    :: FromJSON a
    => ApiClient
    -> ID Form
    -> Str.Map (ID Question) Str.Text
    -> IO a
createFormSubmission client formID submission =
    Raw.createFormSubmission client formID submission >>= Extra.simplifyIO

-- | Submit data to this form using the API. Returns posted submission ID
-- and URL. Submission data is a JSON 'Value' with question IDs.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
createFormSubmissions :: FromJSON a => ApiClient -> ID Form -> Value -> IO a
createFormSubmissions client formID submissions =
    Raw.createFormSubmissions client formID submissions >>= Extra.simplifyIO

-- | List of files uploaded on a form. Returns uploaded file information
-- and URLs on a specific form.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
getFormFiles :: FromJSON a => ApiClient -> ID Form -> IO a
getFormFiles client formID =
    Raw.getFormFiles client formID >>= Extra.simplifyIO

-- | Get list of webhooks for a form.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
getFormWebhooks :: FromJSON a => ApiClient -> ID Form -> IO a
getFormWebhooks client formID =
    Raw.getFormWebhooks client formID >>= Extra.simplifyIO

-- | Add a new webhook. Returns list of webhooks for a specific form.
-- Pass the webhook URL where form data will be posted when form is
-- submitted as 'Str.Text'.
--
-- The 'ID of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
createFormWebhook :: FromJSON a => ApiClient -> ID Form -> Str.Text -> IO a
createFormWebhook client formID url =
    Raw.createFormWebhook client formID url >>= Extra.simplifyIO

-- | Delete a specific webhook of a form. Returns remaining webhook URLs of
-- the form.
--
-- The 'ID' of a 'Form' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
--
-- You can get webhook IDs when you call 'Network.JotForm.Api.getFormWebhooks'.
deleteFormWebhook :: FromJSON a => ApiClient -> ID Form -> ID Webhook -> IO a
deleteFormWebhook client formID whID =
    Raw.deleteFormWebhook client formID whID >>= Extra.simplifyIO
