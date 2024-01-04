{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.ClickhouseV2.Operators where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue
import Kernel.Storage.ClickhouseV2.Internal.Types

(==.) :: forall table value. (ClickhouseTable table, ClickhouseValue value) => Column table value -> value -> Clause table
(==.) column value = column `Is` Eq value

infix 4 ==.

(&&.) :: forall table. ClickhouseTable table => Clause table -> Clause table -> Clause table
(&&.) = And

infixr 3 &&.

(||.) :: forall table. ClickhouseTable table => Clause table -> Clause table -> Clause table
(||.) = Or

infixr 2 ||.

select :: forall db table. Q db table -> Select db table
select = Select

-- FIXME Integer
limit_ :: Int -> Q db table -> Q db table
limit_ limitVal q = q {limitQ = Just $ Limit limitVal}

offset_ :: Int -> Q db table -> Q db table
offset_ offsetVal q = q {offsetQ = Just $ Offset offsetVal}

orderBy_ :: forall db table. ClickhouseTable table => (Columns table -> OrderBy table) -> Q db table -> Q db table
orderBy_ orderByClause q = q {orderByQ = Just $ orderByClause (tableQ q)}

asc :: forall table value. (ClickhouseTable table, ClickhouseValue value) => Column table value -> OrderBy table
asc = OrderBy Asc

desc :: forall table value. (ClickhouseTable table, ClickhouseValue value) => Column table value -> OrderBy table
desc = OrderBy Desc

aggregate_ :: forall db table. (Columns table -> Aggregate table) -> Q db table -> Q db table
aggregate_ aggregateClause q = q {aggregateQ = Just $ aggregateClause (tableQ q)}

group :: forall table value. (ClickhouseTable table, ClickhouseValue value) => Column table value -> Aggregate table
group = GroupByAggregate . GroupBy

all_ ::
  forall db table.
  (ClickhouseDb db, ClickhouseTable table) =>
  FieldModifications table ->
  AllColumns db table
all_ tableMod = AllColumns (mkTableColumns @table tableMod)

filter_ :: (Columns table -> Clause table) -> AllColumns db table -> Q db table
filter_ filterClause (AllColumns columns) =
  Q
    { tableQ = columns,
      whereQ = Where $ filterClause columns,
      aggregateQ = Nothing,
      limitQ = Nothing,
      offsetQ = Nothing,
      orderByQ = Nothing
    }
