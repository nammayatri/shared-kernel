{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.ClickhouseV2.Operators where

import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue
import Kernel.Storage.ClickhouseV2.Internal.ClickhouseColumns
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
in_ column values = column `Is` In values

isNull :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table (Maybe value) -> Clause table
isNull column = Is column NullTerm

isNotNull :: forall a table value. (ClickhouseTable table, ClickhouseValue value) => Column a table (Maybe value) -> Clause table
isNotNull column = Is column NotNullTerm

select :: forall db table ord. ClickhouseTable table => Q db table (Columns 'NOT_AGG table) ord -> Select 'NOT_AGG db table (Columns 'NOT_AGG table) NotGrouped ord
select q = Select q.tableQ NotGrouped q

select_ :: forall a db table cols gr ord. (ClickhouseTable table, ClickhouseColumns a cols) => (Columns 'NOT_AGG table -> (cols, GroupBy a gr)) -> Q db table cols ord -> Select a db table cols gr ord
select_ colsClause q = do
  let (cols, gr) = colsClause q.tableQ
  Select cols gr q

-- FIXME Integer
limit_ :: Int -> Q db table cols ord -> Q db table cols ord
limit_ limitVal q = q {limitQ = Just $ Limit limitVal}

offset_ :: Int -> Q db table cols ord -> Q db table cols ord
offset_ offsetVal q = q {offsetQ = Just $ Offset offsetVal}

orderBy_ :: forall db table cols ord. ClickhouseTable table => (Columns 'NOT_AGG table -> cols -> OrderBy ord) -> Q db table cols NotOrdered -> Q db table cols ord
orderBy_ orderByClause q = q {orderByQ = Just $ orderByClause (tableQ q)}

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
  AllColumns db table
all_ tableMod = AllColumns (mkTableColumns @table tableMod)

filter_ :: (Columns 'NOT_AGG table -> cols -> Clause table) -> AllColumns db table -> Q db table cols NotOrdered
filter_ filterClause (AllColumns table) =
  Q
    { tableQ = table,
      whereQ = Where . filterClause table,
      limitQ = Nothing,
      offsetQ = Nothing,
      orderByQ = Nothing
    }

sum_ :: (ClickhouseTable table, ClickhouseNum value) => Column 'NOT_AGG table value -> Column 'AGG table value
sum_ = Sum

count_ :: (ClickhouseTable table, ClickhouseValue value, ClickhouseValue Int) => Column 'NOT_AGG table value -> Column 'AGG table Int
count_ = Count

distinct :: (ClickhouseTable t, ClickhouseValue v) => Column a t v -> Column a t v
distinct = Distinct

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

timeDiff :: (ClickhouseTable t, ClickhouseValue UTCTime, ClickhouseValue UTCTime, ClickhouseValue Int) => Column a t UTCTime -> Column a t UTCTime -> Column a t Int
timeDiff = TimeDiff
