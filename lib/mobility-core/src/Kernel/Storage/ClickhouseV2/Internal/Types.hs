{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Storage.ClickhouseV2.Internal.Types where

import qualified Data.Aeson.Types as A
import Data.Kind (Constraint)
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue

data Select a db table cols gr ord acols where
  Select :: (ClickhouseTable table, ClickhouseColumns a cols) => cols -> GroupBy a gr -> Q db table cols ord acols -> Select a db table cols gr ord acols

class ClickhouseColumns (a :: IsAggregated) cols where
  type ColumnsType a cols
  showClickhouseColumns :: Proxy a -> cols -> SubQueryLevel -> String
  parseColumns :: Proxy a -> cols -> A.Value -> SubQueryLevel -> Either String (ColumnsType a cols)

newtype RawQuery = RawQuery {getRawQuery :: String}
  deriving newtype (IsString, Semigroup, Monoid)

class ClickhouseQuery expr where
  toClickhouseQuery :: expr -> RawQuery

data IsAggregated = AGG | NOT_AGG

class ClickhouseValue v => ClickhouseNum v

instance (ClickhouseValue v, Num v) => ClickhouseNum v

instance {-# OVERLAPPING #-} (ClickhouseValue v, Num v) => ClickhouseNum (Maybe v)

data Column (a :: IsAggregated) t v where
  Column :: (ClickhouseTable t) => FieldModification t v -> Column 'NOT_AGG t v -- initial column
  Group :: (ClickhouseTable t, ClickhouseValue v) => Column 'NOT_AGG t v -> Column 'AGG t v -- column from groupBy clause
  SubColumn :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> ColumnNumber -> SubQueryLevel -> Column 'NOT_AGG t v -- column synonym will be generated based on these two guys SubQueryLevel and ColumnNumber
  Sum :: (ClickhouseTable t, ClickhouseNum v) => Column 'NOT_AGG t v -> Column 'AGG t v
  Count :: (ClickhouseTable t, ClickhouseValue v, ClickhouseValue Int) => Column 'NOT_AGG t v -> Column 'AGG t Int
  Distinct :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v -- should not be used in where clause
  Max :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v
  Add :: (ClickhouseTable t, ClickhouseNum v) => Column a t v -> Column a t v -> Column a t v
  CoerceNum :: (ClickhouseTable t, ClickhouseNum v1, ClickhouseNum v2) => Column a t v1 -> Column a t v2
  ToDate :: (ClickhouseTable t, ClickhouseValue DateTime, ClickhouseValue Time.Day) => Column a t DateTime -> Column a t Time.Day -- FIXME create some generic constructor for different clickhouse functions
  ToHour :: (ClickhouseTable t, ClickhouseValue DateTime, ClickhouseValue Int) => Column a t DateTime -> Column a t Int
  TimeDiff :: (ClickhouseTable t, ClickhouseValue UTCTime, ClickhouseValue UTCTime, ClickhouseValue Int) => Column a t UTCTime -> Column a t UTCTime -> Column a t Int
  ValColumn :: (ClickhouseTable t, ClickhouseValue v) => v -> Column a t v
  If :: (ClickhouseTable t, ClickhouseValue v) => Column a t Bool -> Column a t v -> Column a t v -> Column a t v
  EqColumn :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v -> Column a t Bool
  ArgMax :: (ClickhouseTable t, ClickhouseValue v1, ClickhouseValue v2) => Column 'NOT_AGG t v1 -> Column 'NOT_AGG t v2 -> Column 'AGG t v1

mkTableColumns :: ClickhouseTable t => FieldModifications t -> Columns 'NOT_AGG t
mkTableColumns = mapTable Column

type Columns a table = table (Column a table)

data Clause table where
  And :: ClickhouseTable table => Clause table -> Clause table -> Clause table
  Or :: ClickhouseTable table => Clause table -> Clause table -> Clause table
  Not :: ClickhouseTable table => Clause table -> Clause table
  Is :: (ClickhouseTable table, ClickhouseValue value) => Column a table value -> Term value -> Clause table
  Val :: (ClickhouseTable table, ClickhouseValue Bool) => Bool -> Clause table

data Term value where
  In :: ClickhouseValue value => [value] -> Term value
  NullTerm :: ClickhouseValue value => Term (Maybe value)
  NotNullTerm :: ClickhouseValue value => Term (Maybe value)
  Eq :: ClickhouseValue value => value -> Term value
  NotEq :: ClickhouseValue value => value -> Term value
  GreaterThan :: ClickhouseValue value => value -> Term value
  GreaterOrEqualThan :: ClickhouseValue value => value -> Term value
  LessThan :: ClickhouseValue value => value -> Term value
  LessOrEqualThan :: ClickhouseValue value => value -> Term value
  Like :: Text -> Term Text

newtype Where table = Where (Clause table)

class IsGroupColumns cols where
  type GroupColumnsType cols
  groupColumns :: cols -> GroupColumnsType cols

instance (ClickhouseTable t, ClickhouseValue v) => IsGroupColumns (Column 'NOT_AGG t v) where
  type GroupColumnsType (Column 'NOT_AGG t v) = Column 'AGG t v
  groupColumns = Group @t @v

instance (ClickhouseTable t, C2 ClickhouseValue v1 v2) => IsGroupColumns (T2 (Column 'NOT_AGG t) v1 v2) where
  type GroupColumnsType (T2 (Column 'NOT_AGG t) v1 v2) = (T2 (Column 'AGG t) v1 v2)
  groupColumns (c1, c2) = (Group @t @v1 c1, Group @t @v2 c2)

instance (ClickhouseTable t, C3 ClickhouseValue v1 v2 v3) => IsGroupColumns (T3 (Column 'NOT_AGG t) v1 v2 v3) where
  type GroupColumnsType (T3 (Column 'NOT_AGG t) v1 v2 v3) = (T3 (Column 'AGG t) v1 v2 v3)
  groupColumns (c1, c2, c3) = (Group @t @v1 c1, Group @t @v2 c2, Group @t @v3 c3)

instance (ClickhouseTable t, C4 ClickhouseValue v1 v2 v3 v4) => IsGroupColumns (T4 (Column 'NOT_AGG t) v1 v2 v3 v4) where
  type GroupColumnsType (T4 (Column 'NOT_AGG t) v1 v2 v3 v4) = (T4 (Column 'AGG t) v1 v2 v3 v4)
  groupColumns (c1, c2, c3, c4) = (Group @t @v1 c1, Group @t @v2 c2, Group @t @v3 c3, Group @t @v4 c4)

instance (ClickhouseTable t, C5 ClickhouseValue v1 v2 v3 v4 v5) => IsGroupColumns (T5 (Column 'NOT_AGG t) v1 v2 v3 v4 v5) where
  type GroupColumnsType (T5 (Column 'NOT_AGG t) v1 v2 v3 v4 v5) = (T5 (Column 'AGG t) v1 v2 v3 v4 v5)
  groupColumns (c1, c2, c3, c4, c5) = (Group @t @v1 c1, Group @t @v2 c2, Group @t @v3 c3, Group @t @v4 c4, Group @t @v5 c5)

instance (ClickhouseTable t, C6 ClickhouseValue v1 v2 v3 v4 v5 v6) => IsGroupColumns (T6 (Column 'NOT_AGG t) v1 v2 v3 v4 v5 v6) where
  type GroupColumnsType (T6 (Column 'NOT_AGG t) v1 v2 v3 v4 v5 v6) = (T6 (Column 'AGG t) v1 v2 v3 v4 v5 v6)
  groupColumns (c1, c2, c3, c4, c5, c6) = (Group @t @v1 c1, Group @t @v2 c2, Group @t @v3 c3, Group @t @v4 c4, Group @t @v5 c5, Group @t @v6 c6)

-- we need to reset group columns for subqueries, thus we can group twice, first time in subquery, second time in main query
class MkSubColumns cols where
  type SubColumnsType cols
  subColumnsValue :: cols -> SubQueryLevel -> SubColumnsType cols

instance (ClickhouseTable t, ClickhouseValue v) => MkSubColumns (Column a t v) where
  type SubColumnsType (Column a t v) = Column 'NOT_AGG t v
  subColumnsValue c l = SubColumn @t @v c 1 l

instance (ClickhouseTable t, C2 ClickhouseValue v1 v2) => MkSubColumns (T2 (Column a t) v1 v2) where
  type SubColumnsType (T2 (Column a t) v1 v2) = (T2 (Column 'NOT_AGG t) v1 v2)
  subColumnsValue (c1, c2) l = (SubColumn @t @v1 c1 1 l, SubColumn @t @v2 c2 2 l)

instance (ClickhouseTable t, C3 ClickhouseValue v1 v2 v3) => MkSubColumns (T3 (Column a t) v1 v2 v3) where
  type SubColumnsType (T3 (Column a t) v1 v2 v3) = (T3 (Column 'NOT_AGG t) v1 v2 v3)
  subColumnsValue (c1, c2, c3) l = (SubColumn @t @v1 c1 1 l, SubColumn @t @v2 c2 2 l, SubColumn @t @v3 c3 3 l)

instance (ClickhouseTable t, C4 ClickhouseValue v1 v2 v3 v4) => MkSubColumns (T4 (Column a t) v1 v2 v3 v4) where
  type SubColumnsType (T4 (Column a t) v1 v2 v3 v4) = (T4 (Column 'NOT_AGG t) v1 v2 v3 v4)
  subColumnsValue (c1, c2, c3, c4) l = (SubColumn @t @v1 c1 1 l, SubColumn @t @v2 c2 2 l, SubColumn @t @v3 c3 3 l, SubColumn @t @v4 c4 4 l)

instance (ClickhouseTable t, C5 ClickhouseValue v1 v2 v3 v4 v5) => MkSubColumns (T5 (Column a t) v1 v2 v3 v4 v5) where
  type SubColumnsType (T5 (Column a t) v1 v2 v3 v4 v5) = (T5 (Column 'NOT_AGG t) v1 v2 v3 v4 v5)
  subColumnsValue (c1, c2, c3, c4, c5) l = (SubColumn @t @v1 c1 1 l, SubColumn @t @v2 c2 2 l, SubColumn @t @v3 c3 3 l, SubColumn @t @v4 c4 4 l, SubColumn @t @v5 c5 5 l)

instance (ClickhouseTable t, C6 ClickhouseValue v1 v2 v3 v4 v5 v6) => MkSubColumns (T6 (Column a t) v1 v2 v3 v4 v5 v6) where
  type SubColumnsType (T6 (Column a t) v1 v2 v3 v4 v5 v6) = (T6 (Column 'NOT_AGG t) v1 v2 v3 v4 v5 v6)
  subColumnsValue (c1, c2, c3, c4, c5, c6) l = (SubColumn @t @v1 c1 1 l, SubColumn @t @v2 c2 2 l, SubColumn @t @v3 c3 3 l, SubColumn @t @v4 c4 4 l, SubColumn @t @v5 c5 5 l, SubColumn @t @v6 c6 6 l)

data NotGrouped

data NoColumns

data GroupBy (a :: IsAggregated) gr where
  GroupBy :: IsGroupColumns gr => gr -> GroupBy 'AGG gr
  Aggregate :: GroupBy 'AGG NoColumns
  NotGrouped :: GroupBy 'NOT_AGG NotGrouped

data OrderBy ord where
  OrderBy :: IsOrderColumns ord => Order -> ord -> OrderBy ord

data NotOrdered

class IsOrderColumns cols

instance IsOrderColumns NotOrdered

instance (ClickhouseTable t, ClickhouseValue v) => IsOrderColumns (Column a t v)

instance (ClickhouseTable t, C2 ClickhouseValue v1 v2) => IsOrderColumns (T2 (Column a t) v1 v2)

instance (ClickhouseTable t, C3 ClickhouseValue v1 v2 v3) => IsOrderColumns (T3 (Column a t) v1 v2 v3)

instance (ClickhouseTable t, C4 ClickhouseValue v1 v2 v3 v4) => IsOrderColumns (T4 (Column a t) v1 v2 v3 v4)

instance (ClickhouseTable t, C5 ClickhouseValue v1 v2 v3 v4 v5) => IsOrderColumns (T5 (Column a t) v1 v2 v3 v4 v5)

instance (ClickhouseTable t, C6 ClickhouseValue v1 v2 v3 v4 v5 v6) => IsOrderColumns (T6 (Column a t) v1 v2 v3 v4 v5 v6)

data Q db table cols ord acols = (ClickhouseDb db) =>
  Q
  { tableQ :: AvailableColumns db table acols,
    subQueryLevelQ :: SubQueryLevel,
    whereQ :: Maybe (cols -> Where table),
    limitQ :: Maybe Limit,
    offsetQ :: Maybe Offset,
    orderByQ :: Maybe (cols -> OrderBy ord),
    selectModifierOverrideQ :: Maybe SelectModifier
  }

newtype Offset = Offset Int

newtype Limit = Limit Int

data Order = Asc | Desc

class HasAvailableColumns (cols :: Type) where
  type AvailableColumnsType cols
  availableColumnsValue :: cols -> AvailableColumnsType cols

instance HasAvailableColumns (AllColumns db table) where
  type AvailableColumnsType (AllColumns db table) = Columns 'NOT_AGG table
  availableColumnsValue (AllColumns cols) = cols

instance HasAvailableColumns (SubSelectColumns db table subcols) where
  type AvailableColumnsType (SubSelectColumns db table subcols) = SubColumnsType subcols
  availableColumnsValue (SubSelectColumns (Select subcols _ q)) = subColumnsValue subcols q.subQueryLevelQ

getAvailableColumnsValue ::
  AvailableColumns db table acols ->
  AvailableColumnsType acols
getAvailableColumnsValue (AvailableColumns acols) = availableColumnsValue acols

data AvailableColumns db table acols where
  AvailableColumns :: (ClickhouseDb db, ClickhouseTable table, HasAvailableColumns acols) => acols -> AvailableColumns db table acols

data AllColumns db table where
  AllColumns :: (ClickhouseDb db, ClickhouseTable table) => Columns 'NOT_AGG table -> AllColumns db table

data SubSelectColumns db table subcols where
  SubSelectColumns :: (ClickhouseDb db, ClickhouseTable table, ClickhouseQuery (Select a db table subcols gr ord acols), MkSubColumns subcols) => Select a db table subcols gr ord acols -> SubSelectColumns db table subcols

type AvailableAllColumns db table = AvailableColumns db table (AllColumns db table)

type AvailableSubSelectColumns db table subcols = AvailableColumns db table (SubSelectColumns db table subcols)

showColumn :: Column a t v -> String
showColumn (Column column) = getFieldModification column
showColumn (Group column) = showColumn column
showColumn (SubColumn _column n l) = getColumnSynonym n l
showColumn (Sum column) = "SUM" <> addBrackets' (showColumn column)
showColumn (Count column) = "COUNT" <> addBrackets' (showColumn column)
showColumn (Distinct column) = "DISTINCT" <> addBrackets' (showColumn column)
showColumn (Max column) = "MAX" <> addBrackets' (showColumn column)
showColumn (Add column1 column2) = addBrackets' (showColumn column1 <> "+" <> showColumn column2)
showColumn (CoerceNum column) = showColumn column
showColumn (ToDate column) = "toDate" <> addBrackets' (showColumn column)
showColumn (ToHour column) = "toHour" <> addBrackets' (showColumn column)
showColumn (TimeDiff column1 column2) = "timeDiff" <> addBrackets' (showColumn column1 <> ", " <> showColumn column2)
showColumn (ValColumn v) = valToString . toClickhouseValue $ v
showColumn (If cond v1 v2) = "if" <> addBrackets' (showColumn cond <> ", " <> showColumn v1 <> ", " <> showColumn v2)
showColumn (EqColumn column1 column2) = addBrackets' $ showColumn column1 <> "=" <> showColumn column2
showColumn (ArgMax arg value) = "argMax" <> addBrackets' (showColumn arg <> ", " <> showColumn value)

addBrackets' :: String -> String
addBrackets' rq = "(" <> rq <> ")"

type T2 (c :: Type -> Type) x1 x2 = (c x1, c x2)

type T3 (c :: Type -> Type) x1 x2 x3 = (c x1, c x2, c x3)

type T4 (c :: Type -> Type) x1 x2 x3 x4 = (c x1, c x2, c x3, c x4)

type T5 (c :: Type -> Type) x1 x2 x3 x4 x5 = (c x1, c x2, c x3, c x4, c x5)

type T6 (c :: Type -> Type) x1 x2 x3 x4 x5 x6 = (c x1, c x2, c x3, c x4, c x5, c x6)

type C2 (c :: Type -> Constraint) x1 x2 = (c x1, c x2)

type C3 (c :: Type -> Constraint) x1 x2 x3 = (c x1, c x2, c x3)

type C4 (c :: Type -> Constraint) x1 x2 x3 x4 = (c x1, c x2, c x3, c x4)

type C5 (c :: Type -> Constraint) x1 x2 x3 x4 x5 = (c x1, c x2, c x3, c x4, c x5)

type C6 (c :: Type -> Constraint) x1 x2 x3 x4 x5 x6 = (c x1, c x2, c x3, c x4, c x5, c x6)

newtype SubQueryLevel = SubQueryLevel {getSubQueryLevel :: Int}
  deriving newtype (Show, Num, Eq)

newtype ColumnNumber = ColumnNumber {getColumnNumber :: Int}
  deriving newtype (Show, Num, Enum, Eq)

getColumnSynonym :: ColumnNumber -> SubQueryLevel -> String
getColumnSynonym n 0 = "res" <> show n
getColumnSynonym n l = "res" <> show n <> "_sub" <> show l
