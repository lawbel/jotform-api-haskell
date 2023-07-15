-- |
-- Module: Network.JotForm
--
-- There are two central modules in this library:
--
-- * "Network.JotForm.Api"
-- * "Network.JotForm.Raw"
--
-- For (almost) every function provided by @Network.JotForm.Api@, there is an
-- alternative function of the same name in @Network.JotForm.Raw@. The reason
-- for this duplication is to provide a simplified API for the common use
-- case, while still allowing the extra functionality the raw bindings give.
-- Typically @Network.JotForm.Raw@ should not be needed, and one can stick
-- to just using @Network.JotForm.Api@.
--
-- For example, calling @Network.JotForm.Api.getUser@ just returns the
-- content of the JSON body of the response:
--
-- > {
-- >     "name": "Example User",
-- >     "email": "example@user.com",
-- >     ...
-- >     "status": "ACTIVE"
-- > }
--
-- In contrast, @Network.JotForm.Raw.getUser@ returns the raw
-- 'Network.HTTP.Client.Response' in full, so you can inspect e.g. the
-- response headers in detail:
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
--
-- This module exports @NetWork.JotForm.Api@ below, not
-- @NetWork.JotForm.Raw@. If you require the raw version of a function you
-- will have to import the \'Raw\' module additionally, and should import it
-- qualified as it uses the same names as in the \'Api\' module.

module Network.JotForm
    ( -- * The Main Interface
      --
      -- | This module exports most things you are likely to need,
      -- such as 'Network.JotForm.Api.getFormsByID' and
      -- 'Network.JotForm.Api.getFormQuestionsByID'.
      module Network.JotForm.Api

      -- * Helper Types
      --
      -- | This module exports helper types such as
      -- 'Network.JotForm.Types.ListOpts' and 'Network.JotForm.Types.ID'
      -- that are used in "Network.JotForm.Api".
    , module Network.JotForm.Types

      -- * Core Logic
      --
      -- | This module implements the core functionality; the most important
      -- thing it exports is 'Network.JotForm.Core.defApiClient'
      -- (and 'Network.JotForm.Core.defApiClientEu') which is needed to
      -- create an 'Network.JotForm.Core.ApiClient'.
    , module Network.JotForm.Core
    )
where

import Network.JotForm.Api
import Network.JotForm.Core
import Network.JotForm.Types
