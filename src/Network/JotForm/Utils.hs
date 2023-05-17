module Network.JotForm.Utils
    ( headerName
    , ascii
    ) where

import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.CaseInsensitive qualified as CaseIns
import Network.HTTP.Types (HeaderName)

headerName :: String -> HeaderName
headerName = CaseIns.mk . ascii

ascii :: String -> Str.ByteString
ascii = Byte.Str.Char8.pack
