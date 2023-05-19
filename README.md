# jotform-api-haskell

## Examples

Print all forms of the user:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Data.Aeson as Json     -- aeson
import           Data.Aeson.Optics      -- aeson-optics
import           Data.Foldable (for_)   -- base
import qualified Data.Text.IO as Text   -- text
import qualified Network.JotForm as JF  -- jotform-api-haskell
import           Optics.Core            -- optics-core

main :: IO ()
main = do
    client <- JF.defaultApiClient "YOUR API KEY"
    forms :: [Json.Value] <- JF.getForms client JF.defaultListOptions
    for_ forms $ \form -> do
        let title = form ^? key "title" % _String
        Text.putStrLn $ maybe "null" id title
```

Get latest 100 submissions ordered by creation date:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Data.Aeson as Json                  -- aeson
import           Data.Aeson.Encode.Pretty            -- aeson-pretty
import qualified Data.ByteString.Lazy.Char8 as Byte  -- bytestring
import           Data.Foldable (for_)                -- base
import qualified Network.JotForm as JF               -- jotform-api-haskell

main :: IO ()
main = do
    client <- JF.defaultApiClient "YOUR API KEY"
    submissions :: [Json.Value] <- JF.getSubmissions client $
        JF.defaultListOptions
            { JF.offset = Just 0
            , JF.limit = Just 100
            , JF.orderBy = Just "created_at" }
    for_ submissions $ \sub -> do
        Byte.putStrLn $ encodePretty sub
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
