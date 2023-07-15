{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.JotForm.Raw.Form
--
-- Summary of functions in this module:
--
-- +--------------------------------+----------------------+------------------------+-------------------------+---------------------+
-- | Endpoint \\ Method             | GET                  | POST                   | PUT                     | DELETE              |
-- +================================+======================+========================+=========================+=====================+
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

module Network.JotForm.Raw.Form
    ( -- * \/form

      -- ** \/{id}
      getForm

      -- *** \/questions
    , getFormQuestions

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
import Data.Map.Strict qualified as Map.Str
import Data.Map.Strict qualified as Str (Map)
import Data.Text qualified as Str (Text)
import Data.Text qualified as Text.Str
import Network.HTTP.Client (Response)
import Network.HTTP.Types.Method qualified as Method
import Network.JotForm.Base ((&=))
import Network.JotForm.Base qualified as Base
import Network.JotForm.Core (ApiClient)
import Network.JotForm.Core qualified as Core
import Network.JotForm.Extra qualified as Extra
import Network.JotForm.Types (Form, ID, ListOpts, Question, Webhook)
import Network.JotForm.Types qualified as Types

-- | Get basic information about a form - form ID, status, update and
-- creation dates, submission count etc.
getForm :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getForm client (Types.MkID formID) = Extra.basicGet path client
  where
    path = "/form/" <> formID

-- | Get a list of all questions on a form.
getFormQuestions :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormQuestions client (Types.MkID formID) = Extra.basicGet path client
  where
    path = "/form/" <> formID <> "/questions"

-- | Returns details about a question like whether it is required and if
-- there is any validation.
getFormQuestion
    :: FromJSON a => ApiClient -> ID Form -> ID Question -> IO (Response a)
getFormQuestion client (Types.MkID formID) (Types.MkID qID) =
    Extra.basicGet path client
  where
    path = "/form/" <> formID <> "/question/" <> qID

-- | Returns submissions of a specific form.
getFormSubmissions
    :: FromJSON a => ApiClient -> ID Form -> ListOpts -> IO (Response a)
getFormSubmissions client (Types.MkID formID) options =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = Types.listOptsToQuery options
            , Core.body = Text.Str.empty
            , Core.headers = []
            , Core.method = Method.methodGet
            }
  where
    path = "/form/" <> formID <> "/submissions"

-- | Submit data to this form using the API. Returns posted submission ID
-- and URL. Pass submission data as a Map from with question IDs.
createFormSubmission
    :: FromJSON a
    => ApiClient
    -> ID Form
    -> Str.Map (ID Question) Str.Text
    -> IO (Response a)
createFormSubmission client (Types.MkID formID) submission =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = []
            , Core.body = Base.renderQueryText query
            , Core.headers = [Core.urlEncode]
            , Core.method = Method.methodPost
            }
  where
    query = Types.optionsToQuery $ Types.MkOptions $ mapKeys submission
    mapKeys = Map.Str.mapKeys $ Base.questionName . Types.unID
    path = "/form/" <> formID <> "/submissions"

-- | Submit data to this form using the API. Returns posted submission ID
-- and URL. Submission data is a JSON 'Value' with question IDs.
createFormSubmissions
    :: FromJSON a => ApiClient -> ID Form -> Value -> IO (Response a)
createFormSubmissions client (Types.MkID formID) submissions =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = []
            , Core.body = Base.encodeText submissions
            , Core.headers = []
            , Core.method = Method.methodPut
            }
  where
    path = "/form/" <> formID <> "/submissions"

-- | List of files uploaded on a form. Returns uploaded file information
-- and URLs on a specific form.
getFormFiles :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormFiles client (Types.MkID formID) = Extra.basicGet path client
  where
    path = "/form/" <> formID <> "/files"

-- | Get list of webhooks for a form.
getFormWebhooks :: FromJSON a => ApiClient -> ID Form -> IO (Response a)
getFormWebhooks client (Types.MkID formID) = Extra.basicGet path client
  where
    path = "/form/" <> formID <> "/webhooks"

-- | Add a new webhook. Returns list of webhooks for a specific form.
-- Pass the webhook URL where form data will be posted when form is
-- submitted as 'Str.Text'.
createFormWebhook
    :: FromJSON a => ApiClient -> ID Form -> Str.Text -> IO (Response a)
createFormWebhook client (Types.MkID formID) url =
    Core.fetchJson client $
        Core.MkParams
            { Core.path = path
            , Core.query = []
            , Core.body = Base.renderQueryText ["webhookURL" &= url]
            , Core.headers = []
            , Core.method = Method.methodPost
            }
  where
    path = "/form/" <> formID <> "/webhooks"

-- | Delete a specific webhook of a form. Returns remaining webhook URLs of
-- the form.
deleteFormWebhook
    :: FromJSON a => ApiClient -> ID Form -> ID Webhook -> IO (Response a)
deleteFormWebhook client (Types.MkID formID) (Types.MkID whID) =
    Extra.basicDelete path client
  where
    path = "/form/" <> formID <> "/webhooks/" <> whID
