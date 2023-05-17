module Network.JotForm.Utils
    ( mkHeader
    , ascii
    ) where

import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Char8 qualified as Byte.Str.Char8
import Data.CaseInsensitive qualified as CaseIns
import Network.HTTP.Types (Header)

mkHeader :: String -> Str.ByteString -> Header
mkHeader key val = (CaseIns.mk $ ascii key, val)

ascii :: String -> Str.ByteString
ascii = Byte.Str.Char8.pack
