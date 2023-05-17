module Network.JotForm.Utils
    ( headerName
    , ascii
    , updateHeaders
    ) where

import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.CaseInsensitive qualified as CaseIns
import Network.HTTP.Client (Request)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (HeaderName, RequestHeaders)

headerName :: String -> HeaderName
headerName = CaseIns.mk . ascii

ascii :: String -> Str.ByteString
ascii = Byte.Str.Char8.pack

updateHeaders :: (RequestHeaders -> RequestHeaders) -> Request -> Request
updateHeaders modify request =
    request
        { Client.requestHeaders =
            modify $ Client.requestHeaders request
        }
