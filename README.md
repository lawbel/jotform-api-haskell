# jotform-api-haskell

## Examples

Print all forms of the user:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Data.Aeson as Json     -- aeson
import           Data.Aeson.Optics      -- aeson-optics
import           Data.Foldable          -- base
import qualified Network.JotForm as JF  -- jotform-api-haskell
import           Optics.Core            -- optics-core
import           Text.Printf            -- base

main :: IO ()
main = do
    -- if account is in EU safe mode, use 'JF.defApiClientEu' instead
    client <- JF.defApiClient "YOUR API KEY"
    forms :: [Json.Value] <- JF.getForms client JF.defListOpts
    for_ forms $ \form -> do
        let title = (form ^? key "title" % _String)            =? "-"
        let total = (form ^? key "count" % _String % _Integer) =? 0
        let new   = (form ^? key "new"   % _String % _Integer) =? 0
        printf "%s (total: %d, new: %d)\n" title total new
  where
    optional =? def = maybe def id optional
```

Get latest 100 submissions ordered by creation date:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Data.Aeson as Json                  -- aeson
import           Data.Aeson.Encode.Pretty            -- aeson-pretty
import qualified Data.ByteString.Lazy.Char8 as Byte  -- bytestring
import           Data.Foldable                       -- base
import qualified Network.JotForm as JF               -- jotform-api-haskell

main :: IO ()
main = do
    client <- JF.defApiClient "YOUR API KEY"
    submissions :: [Json.Value] <- JF.getSubmissions client $
        JF.defListOpts
            { JF.offset = Just 0
            , JF.limit = Just 100
            , JF.orderBy = Just "created_at" }
    for_ submissions $ \sub -> do
        Byte.putStrLn $ encodePretty sub
```

Get submissions/forms matching a filter:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Data.Aeson             -- aeson
import qualified Network.JotForm as JF  -- jotform-api-haskell

main :: IO ()
main = do
    client <- JF.defApiClient "YOUR API KEY"

    let submissionFilter = object
            [ "created_at:gt" .= ("2020-02-20" :: String) ]
    submissions :: [Value] <- JF.getSubmissions client $
        JF.defListOpts { JF.filters = Just submissionFilter }
    print submissions

    let formFilter = object
            [ "new:gt" .= ("0" :: String)
            , "status" .= ("ENABLED" :: String) ]
    forms :: [Value] <- JF.getForms client $
        JF.defListOpts { JF.filters = Just formFilter }
    print forms
```

## Testing

To run a test suite with equivalent code from the above examples:

- make a file `test-api-key.txt` and save a jotform API key in it (or
  `test-api-key-eu.txt` if the associated account is in EU safe mode)
- run `cabal run examples` or `cabal test examples` (the former shows
  the full output from running the tests)

## Nix

For those so inclined, there is a `flake.nix` file set up. So you can
do (amongst other things):

- `nix develop` to get a development shell
- `nix develop --command $EDITOR` to open your editor of choice, with all the
  development tools in place
