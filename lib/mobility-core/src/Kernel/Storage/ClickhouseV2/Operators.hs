{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.ClickhouseV2.Operators where

import qualified Data.List.NonEmpty as NE
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue
import Kernel.Storage.ClickhouseV2.Internal.ClickhouseColumns ()
import Kernel.Storage.ClickhouseV2.Internal.Types

(==.) :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table value -> value -> Clause table
(==.) column value = column `Is` Eq value

(!=.) :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table value -> value -> Clause table
(!=.) column value = column `Is` NotEq value

(>.) :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table value -> value -> Clause table
(>.) column value = column `Is` GreaterThan value

(<.) :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table value -> value -> Clause table
(<.) column value = column `Is` LessThan value

(>=.) :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table value -> value -> Clause table
(>=.) column value = column `Is` GreaterOrEqualThan value

(<=.) :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table value -> value -> Clause table
(<=.) column value = column `Is` LessOrEqualThan value

infix 4 ==., >., <., >=., <=.

(&&.) :: forall table. ClickhouseTable table => Clause table -> Clause table -> Clause table
(&&.) = And

infixr 3 &&.

(||.) :: forall table. ClickhouseTable table => Clause table -> Clause table -> Clause table
(||.) = Or

infixr 2 ||.

not_ :: forall table. ClickhouseTable table => Clause table -> Clause table
not_ = Not

val_ :: forall table. (ClickhouseTable table, ClickhouseValue Bool) => Bool -> Clause table
val_ = Val

valColumn :: forall a t v. (ClickhouseTable t, ClickhouseValue v) => v -> Column a t v
valColumn = ValColumn

in_ :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table value -> [value] -> Clause table
in_ _column [] = val_ False
in_ column (value1 : values) = column `Is` In (value1 NE.:| values)

isNull :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table (Maybe value) -> Clause table
isNull column = Is column NullTerm

isNotNull :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table (Maybe value) -> Clause table
isNotNull column = Is column NotNullTerm

select ::
  forall db table ord.
  ClickhouseTable table =>
  Q db table (Columns 'NOT_AGG table) ord (AllColumns db table) ->
  Select 'NOT_AGG db table (Columns 'NOT_AGG table) NotGrouped ord (AllColumns db table)
select q = Select (getAvailableColumnsValue q.tableQ) NotGrouped q

select_ ::
  forall a db table cols gr ord acols.
  (ClickhouseTable table, ClickhouseColumns a cols) =>
  (AvailableColumnsType acols -> (cols, GroupBy a gr)) ->
  Q db table cols ord acols ->
  Select a db table cols gr ord acols
select_ colsClause q = do
  let (cols, gr) = colsClause (getAvailableColumnsValue q.tableQ)
  Select cols gr q

-- FIXME Integer
limit_ :: Int -> Q db table cols ord subsel -> Q db table cols ord subsel
limit_ limitVal q = q {limitQ = Just $ Limit limitVal}

offset_ :: Int -> Q db table cols ord subsel -> Q db table cols ord subsel
offset_ offsetVal q = q {offsetQ = Just $ Offset offsetVal}

selectModifierOverride :: SelectModifier -> Q db table cols ord subsel -> Q db table cols ord subsel
selectModifierOverride selectModifier q = q {selectModifierOverrideQ = Just selectModifier}

orderBy_ ::
  forall db table cols ord acols.
  ClickhouseTable table =>
  (AvailableColumnsType acols -> cols -> OrderBy ord) ->
  Q db table cols NotOrdered acols ->
  Q db table cols ord acols
orderBy_ orderByClause q = q {orderByQ = Just $ orderByClause $ getAvailableColumnsValue (tableQ q)}

asc :: forall ord. IsOrderColumns ord => ord -> OrderBy ord
asc = OrderBy Asc

desc :: forall ord. IsOrderColumns ord => ord -> OrderBy ord
desc = OrderBy Desc

groupBy :: forall cols gr. IsGroupColumns gr => gr -> (GroupColumnsType gr -> cols) -> (cols, GroupBy 'AGG gr)
groupBy gr mkCols = (mkCols (groupColumns gr), GroupBy gr)

aggregate :: forall cols. cols -> (cols, GroupBy 'AGG NoColumns)
aggregate cols = (cols, Aggregate)

notGrouped :: cols -> (cols, GroupBy 'NOT_AGG NotGrouped)
notGrouped cols = (cols, NotGrouped)

all_ ::
  forall db table.
  (ClickhouseDb db, ClickhouseTable table) =>
  FieldModifications table ->
  (AvailableAllColumns db table, SubQueryLevel)
all_ tableMod = (AvailableColumns $ AllColumns (mkTableColumns @table tableMod), 0)

subSelect_ ::
  forall a db table subcols gr ord acols.
  ( ClickhouseDb db,
    ClickhouseTable table,
    ClickhouseQuery (Select a db table subcols gr ord acols),
    MkSubColumns subcols
  ) =>
  Select a db table subcols gr ord acols ->
  (AvailableSubSelectColumns db table subcols, SubQueryLevel)
subSelect_ s@(Select _cols _gr q) = (AvailableColumns . SubSelectColumns $ s, q.subQueryLevelQ + 1)

filter_ ::
  ClickhouseDb db =>
  (AvailableColumnsType acols -> cols -> Clause table) ->
  (AvailableColumns db table acols, SubQueryLevel) ->
  Q db table cols NotOrdered acols
filter_ filterClause (table, level) =
  Q
    { tableQ = table,
      subQueryLevelQ = level,
      whereQ = Just $ Where . filterClause (getAvailableColumnsValue table),
      limitQ = Nothing,
      offsetQ = Nothing,
      orderByQ = Nothing,
      selectModifierOverrideQ = Nothing
    }

emptyFilter ::
  ClickhouseDb db =>
  (AvailableColumns db table acols, SubQueryLevel) ->
  Q db table cols NotOrdered acols
emptyFilter (table, level) =
  Q
    { tableQ = table,
      subQueryLevelQ = level,
      whereQ = Nothing,
      limitQ = Nothing,
      offsetQ = Nothing,
      orderByQ = Nothing,
      selectModifierOverrideQ = Nothing
    }

sum_ :: (ClickhouseTable table, ClickhouseNum value) => Column 'NOT_AGG table value -> Column 'AGG table value
sum_ = Sum

avg_ :: (ClickhouseTable table, ClickhouseNum value) => Column 'NOT_AGG table value -> Column 'AGG table value
avg_ = Avg

count_ :: (ClickhouseTable table, ClickhouseValue value, ClickhouseValue Int) => Column 'NOT_AGG table value -> Column 'AGG table Int
count_ = Count

distinct :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v
distinct = Distinct

max :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v
max = Max

(+.) :: (ClickhouseTable t, ClickhouseNum v) => Column a t v -> Column a t v -> Column a t v
(+.) = Add

infixl 6 +.

-- for example we can use for columns sum with different types
unsafeCoerceNum :: forall v1 v2 a t. (ClickhouseTable t, ClickhouseNum v1, ClickhouseNum v2) => Column a t v1 -> Column a t v2
unsafeCoerceNum = CoerceNum

whenJust_ :: ClickhouseTable t => Maybe a -> (a -> Clause t) -> Clause t
whenJust_ (Just a) mkClause = mkClause a
whenJust_ Nothing _ = val_ True

toDate :: (ClickhouseTable t, ClickhouseValue DateTime, ClickhouseValue Time.Day) => Column a t DateTime -> Column a t Time.Day
toDate = ToDate

toHour :: (ClickhouseTable t, ClickhouseValue DateTime, ClickhouseValue Int) => Column a t DateTime -> Column a t Int
toHour = ToHour

toStartOfWeek :: (ClickhouseTable t, ClickhouseValue Time.Day, ClickhouseValue Int) => Column a t Time.Day -> Column a t Int -> Column a t Time.Day
toStartOfWeek = ToStartOfWeek

toStartOfMonth :: (ClickhouseTable t, ClickhouseValue Time.Day) => Column a t Time.Day -> Column a t Time.Day
toStartOfMonth = ToStartOfMonth

timeDiff :: (ClickhouseTable t, ClickhouseValue UTCTime, ClickhouseValue UTCTime, ClickhouseValue Int) => Column a t UTCTime -> Column a t UTCTime -> Column a t Int
timeDiff = TimeDiff

if_ :: (ClickhouseTable t, ClickhouseValue v) => Column a t Bool -> Column a t v -> Column a t v -> Column a t v
if_ = If

case_ :: (ClickhouseTable t, ClickhouseValue v) => NonEmpty (Column a t Bool, Column a t v) -> Column a t v -> Column a t v
case_ = Case

(==..) :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v -> Column a t Bool
(==..) = EqColumn

(&&..) :: ClickhouseTable t => Column a t Bool -> Column a t Bool -> Column a t Bool
(&&..) = AndColumn

(||..) :: ClickhouseTable t => Column a t Bool -> Column a t Bool -> Column a t Bool
(||..) = OrColumn

(>..) :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v -> Column a t Bool
(>..) = Greater

(<..) :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v -> Column a t Bool
(<..) = Less

(>=..) :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v -> Column a t Bool
(>=..) = GreaterOrEqual

(<=..) :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v -> Column a t Bool
(<=..) = LessOrEqual

infix 4 ==.., >.., <.., >=.., <=..

infixr 3 &&..

infixr 2 ||..

-- | Calculates the 'arg' value for a maximum 'val' value.
-- If there are multiple rows with equal 'val' being the maximum, which of the associated 'arg' is returned is not deterministic
argMax ::
  (ClickhouseTable t, ClickhouseValue v1, ClickhouseValue v2) =>
  Column 'NOT_AGG t v1 -> -- 'arg'
  Column 'NOT_AGG t v2 -> -- 'val'
  Column 'AGG t v1
argMax = ArgMax
