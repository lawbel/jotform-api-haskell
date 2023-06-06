{-# LANGUAGE OverloadedStrings #-}

module Network.JotForm.Utils
    ( -- * Headers
      headerName
    , updateHeaders

      -- * Text
    , showText
    , elemIndexText

      -- * JSON
    , encodeText

      -- * Date / Time
    , formatDateText
    , renderDateJF

      -- * QueryText
    , renderQueryText
    , renderQueryBytes
    ) where

import Data.Aeson (ToJSON)
import Data.Aeson.Text qualified as Json.Text
import Data.ByteString qualified as Byte.Str
import Data.ByteString qualified as Str (ByteString)
import Data.ByteString.Builder qualified as Byte.Lazy.Bldr
import Data.CaseInsensitive qualified as CaseIns
import Data.Text qualified as Str (Text)
import Data.Text qualified as Text.Str
import Data.Text.Encoding qualified as Text.Str.Enc
import Data.Text.Lazy qualified as Text.Lazy
import Data.Time (FormatTime, TimeLocale, Day)
import Data.Time qualified as Time
import Network.HTTP.Client (Request)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (HeaderName, QueryText, RequestHeaders)
import Network.HTTP.Types.URI qualified as URI

headerName :: Str.Text -> HeaderName
headerName = CaseIns.mk . Text.Str.Enc.encodeUtf8

elemIndexText :: Char -> Str.Text -> Maybe Int
elemIndexText c = Text.Str.findIndex (== c)

showText :: Show a => a -> Str.Text
showText = Text.Str.pack . show

updateHeaders :: (RequestHeaders -> RequestHeaders) -> Request -> Request
updateHeaders modify request =
    request
        { Client.requestHeaders =
            modify $ Client.requestHeaders request
        }

encodeText :: ToJSON a => a -> Str.Text
encodeText = Text.Lazy.toStrict . Json.Text.encodeToLazyText

-- | Convenience function - as we never need to render the '?' at the start
-- of the query, it is easier to not have to specify that at every call site.
renderQueryText :: QueryText -> Str.Text
renderQueryText = Text.Str.Enc.decodeUtf8 . renderQueryBytes

renderQueryBytes :: QueryText -> Str.ByteString
renderQueryBytes =
    Byte.Str.toStrict
        . Byte.Lazy.Bldr.toLazyByteString
        . URI.renderQueryText False

formatDateText :: FormatTime t => TimeLocale -> Str.Text -> t -> Str.Text
formatDateText locale fmt time =
    Text.Str.pack $ Time.formatTime locale (Text.Str.unpack fmt) time

renderDateJF :: Day -> Str.Text
renderDateJF = formatDateText Time.defaultTimeLocale "%m/%d/%Y"
