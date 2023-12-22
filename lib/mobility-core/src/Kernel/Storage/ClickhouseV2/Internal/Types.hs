{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.ClickhouseV2.Internal.Types where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.ClickhouseValue

newtype Column table value = Column {getColumn :: FieldModification table value}

mkTableColumns :: ClickhouseTable t => FieldModifications t -> Columns t
mkTableColumns = mapTable Column

type Columns table = table (Column table)

data Clause table where
  And :: ClickhouseTable table => Clause table -> Clause table -> Clause table
  Or :: ClickhouseTable table => Clause table -> Clause table -> Clause table
  Is :: (ClickhouseTable table, ClickhouseValue value) => Column table value -> Term value -> Clause table

data Term value where
  In :: ClickhouseValue value => [value] -> Term value
  Eq :: ClickhouseValue value => value -> Term value
  NotEq :: ClickhouseValue value => value -> Term value
  GreaterThan :: ClickhouseValue value => value -> Term value
  GreaterOrEqualThan :: ClickhouseValue value => value -> Term value
  LessThan :: ClickhouseValue value => value -> Term value
  LessOrEqualThan :: ClickhouseValue value => value -> Term value
  Like :: Text -> Term Text

newtype Where table = Where (Clause table)

newtype Aggregate table = GroupByAggregate (GroupBy table)

data GroupBy table where
  GroupBy :: (ClickhouseTable table, ClickhouseValue value) => Column table value -> GroupBy table

data OrderBy table where
  OrderBy :: (ClickhouseTable table, ClickhouseValue value) => Order -> Column table value -> OrderBy table

data Q db table = (ClickhouseDb db) =>
  Q
  { tableQ :: Columns table,
    whereQ :: Where table,
    aggregateQ :: Maybe (Aggregate table),
    limitQ :: Maybe Limit,
    offsetQ :: Maybe Offset,
    orderByQ :: Maybe (OrderBy table)
  }

newtype Offset = Offset Int

newtype Limit = Limit Int

data Order = Asc | Desc

newtype Select db table = Select (Q db table) -- constraint work

data AllColumns db table where
  AllColumns :: (ClickhouseDb db, ClickhouseTable table) => Columns table -> AllColumns db table
