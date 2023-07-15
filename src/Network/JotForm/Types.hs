{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.JotForm.Types

module Network.JotForm.Types
    ( -- * ID Type
      ID (..)

      -- ** Tags
    , Form
    , Question
    , Webhook

      -- * ListOpts
    , ListOpts (..)
    , defListOpts
    , listOptsToQuery

      -- * HistoryOpts
    , HistoryOpts (..)
    , historyOptsToQuery
    , defHistoryOpts

      -- ** Action
    , UserAction (..)
    , renderUserAction

      -- ** DateFilter
    , DateFilter (..)
    , dateFilterToQuery

      -- ** DateRange
    , DateRange (..)
    , renderDateRange

      -- ** SortBy
    , SortBy (..)
    , renderSortBy

      -- * Options
    , Options (..)
    , optionsToQuery
    )
where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, Value)
import Data.Bifunctor (second)
import Data.Map.Strict qualified as Map.Str
import Data.Map.Strict qualified as Str (Map)
import Data.String (IsString)
import Data.Text qualified as Str (Text)
import Data.These (These (..))
import Data.Time (Day)
import Network.HTTP.Types (QueryText)
import Network.JotForm.Base ((&=))
import Network.JotForm.Base qualified as Base

-- $tags
--
-- These types have no constructors (like 'Data.Void.Void' from @base@), so no
-- values can be constructed for them. They are used with 'ID' as a tag to
-- indicate what kind of ID is expected for the arguments to various functions.

-- | A collection of key-value options.
newtype Options = MkOptions
    { unOptions :: Str.Map Str.Text Str.Text
    }
    deriving (Eq, Ord, Show, Read)

newtype ID ty = MkID {unID :: Str.Text}
    deriving (Eq, Ord, Show, Read)
    deriving
        ( IsString
        , Semigroup
        , Monoid
        , FromJSON
        , FromJSONKey
        , ToJSON
        , ToJSONKey
        )
        via Str.Text

-- | The \'form ID\' is the numbers you see on a form URL. You can get
-- form IDs when you call 'Network.JotForm.Api.getFormsByID'
-- or 'Network.JotForm.Api.getForms'.
data Form

-- | Identifier for each question on a form. You can get a list of
-- question IDs from 'Network.JotForm.Api.getFormQuestionsByID'
-- or 'Network.JotForm.Api.getFormQuestions'.
data Question

data Webhook

-- | A bundle of options that are re-used in a couple of places in the API
-- where it can potentially return a (very) long list of values.
--
-- * if @offset = Nothing@ then the default value used by JotForm is 20
-- * if @limit = Nothing@ then the default value used by JotForm is 20
-- * the maximum @limit@ is 1000
data ListOpts = MkListOpts
    { offset :: Maybe Int
    -- ^ Start of each result list; useful for pagination.
    , limit :: Maybe Int
    -- ^ Number of results in each result list.
    , filters :: Maybe Value
    -- ^ Filters the query results to fetch a specific submissions range.
    , orderBy :: Maybe Str.Text
    -- ^ Order results by a field name.
    }
    deriving (Eq, Ord, Show, Read)

data UserAction
    = AllActions
    | UserCreation
    | UserLogin
    | FormCreation
    | FormUpdate
    | FormDelete
    | FormPurge
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data DateFilter
    = -- | The empty filter.
      NoFilter
    | -- | Limit results by a given preset 'DateRange'.
      RangeFilter DateRange
    | -- | Represents a filter between the start/end dates given (if any).
      -- Given @'StartEndFilter' these@, the possible cases are:
      --
      -- * if @these == 'This' start@, then limit results to after @start@.
      -- * if @these == 'That' end@, then limit results to before @end@.
      -- * if @these == 'These' start end@, then limit results to after
      --   @start@ and before @end@.
      StartEndFilter (These Day Day)
    deriving (Eq, Ord, Show, Read)

data DateRange
    = LastWeek
    | LastMonth
    | Last3Months
    | Last6Months
    | LastYear
    | AllTime
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data SortBy
    = -- | Sort ascending.
      SortAsc
    | -- | Sort descending.
      SortDesc
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

-- | A bundle of options provided for convenience when requesting a
-- history\/queue.
data HistoryOpts = MkHistoryOpts
    { action :: Maybe UserAction
    -- ^ Filter results by activity performed. Default is 'AllActions'.
    , dateFilter :: DateFilter
    -- ^ Limit results by a date filter.
    , sortBy :: Maybe SortBy
    -- ^ Lists results by ascending and descending order.
    }
    deriving (Eq, Ord, Show, Read)

renderUserAction :: UserAction -> Str.Text
renderUserAction = \case
    AllActions -> "all"
    UserCreation -> "userCreation"
    UserLogin -> "userLogin"
    FormCreation -> "formCreation"
    FormUpdate -> "formUpdate"
    FormDelete -> "formDelete"
    FormPurge -> "formPurge"

renderSortBy :: SortBy -> Str.Text
renderSortBy = \case
    SortAsc -> "ASC"
    SortDesc -> "DESC"

renderDateRange :: DateRange -> Str.Text
renderDateRange = \case
    LastWeek -> "lastWeek"
    LastMonth -> "lastMonth"
    Last3Months -> "last3Months"
    Last6Months -> "last6Months"
    LastYear -> "lastYear"
    AllTime -> "all"

-- | Conversion function from 'DateFilter' to 'QueryText' - not something that
-- end users will normally need, but provided just in case.
dateFilterToQuery :: DateFilter -> QueryText
dateFilterToQuery = \case
    NoFilter -> []
    RangeFilter range -> ["date" &= renderDateRange range]
    StartEndFilter (This start) -> [startQuery start]
    StartEndFilter (That end) -> [endQuery end]
    StartEndFilter (These start end) -> [startQuery start, endQuery end]
  where
    startQuery day = "startDate" &= Base.renderDateJF day
    endQuery day = "endDate" &= Base.renderDateJF day

-- | A default 'ListOpts' value; it simply sets 'Nothing' as the value
-- of each option, so that none of these options is specified.
defListOpts :: ListOpts
defListOpts =
    MkListOpts
        { offset = Nothing
        , limit = Nothing
        , filters = Nothing
        , orderBy = Nothing
        }

-- | Conversion function from 'ListOpts' to 'QueryText' - not something that
-- end users will normally need, but provided just in case.
listOptsToQuery :: ListOpts -> QueryText
listOptsToQuery options = do
    (key, mVal) <- keys `zip` vals
    case mVal of
        Nothing -> empty
        Just val -> pure (key &= val)
  where
    keys = ["offset", "limit", "filter", "orderby"]
    vals =
        [ Base.showText <$> offset options
        , Base.showText <$> limit options
        , Base.encodeText <$> filters options
        , orderBy options
        ]

-- | Conversion function from 'Options' to 'QueryText' - not something that
-- end users will normally need, but provided just in case.
optionsToQuery :: Options -> QueryText
optionsToQuery = fmap (second Just) . Map.Str.toList . unOptions

historyOptsToQuery :: HistoryOpts -> QueryText
historyOptsToQuery options =
    dateFilterToQuery (dateFilter options) <> otherQueries
  where
    otherQueries = do
        (key, mVal) <- keys `zip` vals
        case mVal of
            Nothing -> empty
            Just val -> pure (key &= val)
    keys = ["action", "sortBy"]
    vals =
        [ renderUserAction <$> action options
        , renderSortBy <$> sortBy options
        ]

-- | A default 'HistoryOpts' value; it simply sets 'Nothing' / 'NoFilter' as
-- the value of each option, so that none of the options are specified.
defHistoryOpts :: HistoryOpts
defHistoryOpts =
    MkHistoryOpts
        { action = Nothing
        , dateFilter = NoFilter
        , sortBy = Nothing
        }
