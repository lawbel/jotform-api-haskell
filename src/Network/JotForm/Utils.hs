module Network.JotForm.Utils
    ( headerName
    , ascii
    , showAscii
    , updateHeaders
    , encodeStrict
    , renderQuery
    ) where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Json
import Data.ByteString qualified as Byte.Str
import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.CaseInsensitive qualified as CaseIns
import Network.HTTP.Client (Request)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (HeaderName, Query, RequestHeaders)
import Network.HTTP.Types.URI qualified as URI

headerName :: String -> HeaderName
headerName = CaseIns.mk . ascii

ascii :: String -> Str.ByteString
ascii = Byte.Str.Char8.pack

showAscii :: Show a => a -> Str.ByteString
showAscii = ascii . show

updateHeaders :: (RequestHeaders -> RequestHeaders) -> Request -> Request
updateHeaders modify request =
    request
        { Client.requestHeaders =
            modify $ Client.requestHeaders request
        }

encodeStrict :: ToJSON a => a -> Str.ByteString
encodeStrict = Byte.Str.toStrict . Json.encode

-- | Convenience function - as we never need to render the '?' at the start
-- of the query, it is easier to not have to specify that at every call site.
renderQuery :: Query -> Str.ByteString
renderQuery = URI.renderQuery False
