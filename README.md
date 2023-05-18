# jotform-api-python

## Examples

Print all forms of the user:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Data.Aeson as Json          -- aeson
import           Data.Aeson.Optics           -- aeson-optics
import           Data.Foldable (for_)        -- base
import qualified Data.Text.IO as Text.IO     -- text
import qualified Network.JotForm as JotForm  -- jotform-api-haskell
import           Optics.Core                 -- optics-core

main :: IO ()
main = do
    client <- JotForm.defaultApiClient "YOUR API KEY"
    forms :: [Json.Value] <-
        JotForm.getForms client JotForm.defaultListOptions
    for_ forms $ \form -> do
        let title = form ^? key "title" % _String
        Text.IO.putStrLn $ maybe "null" id title
```

Get latest 100 submissions ordered by creation date:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Data.Aeson as Json                  -- aeson
import           Data.Aeson.Encode.Pretty            -- aeson-pretty
import qualified Data.ByteString.Lazy.Char8 as Byte  -- bytestring
import           Data.Foldable (for_)                -- base
import qualified Network.JotForm as JotForm          -- jotform-api-haskell

main :: IO ()
main = do
    client <- JotForm.defaultApiClient "YOUR API KEY"
    submissions :: [Json.Value] <- JotForm.getSubmissions client options
    for_ submissions $ \sub -> do
        Byte.putStrLn $ encodePretty sub
  where
    options = JotForm.defaultListOptions
        { JotForm.offset = Just 0
        , JotForm.limit = Just 100
        , JotForm.orderBy = Just JotForm.ByCreatedAt
        }
```

## Testing

To run a test suite with equivalent code from the above examples:

- make a file `test-api-key.txt` and save a jotform API key in it (or
  `test-api-key-eu.txt` if the associated account is in EU safe mode)
- run `cabal run examples` or `cabal test examples` (the former shows
  the full output from running the tests)

## Nix

For those so inclined, there is a `flake.nix` file set up. So you can
do (amongt other things):

- `nix develop` to get a development shell
- `nix develop --run $EDITOR` to open your editor of choice, with all the
  development tools in place
